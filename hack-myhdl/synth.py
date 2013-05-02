# MyHDL code for a Moog-style synth done in DSP on a Xilinx FPGA

import unittest
import sys
from myhdl import Signal, delay, Simulation, always_comb, \
    instance, intbv, bin, toVerilog, toVHDL, always, now, traceSignals

from wavegen import waveform_generator
from param_loading import (
    get_nibbles,
    DaisyChain,
    bitfields,
    daisy_chain_driver,
    param_clock_driver
)
from output_stage import delta_sigma_dac
from amps_filters import vca
from envgen import adsr
from config import (
    MHZ,
    AUDIO_RATE,
    DIVIDER,
    PDIVIDER,
    N,
    WHOLE,
    HALF,
    MASK,
    DELTA_PHASE,
    SIMLEN,
    SIMTIME,
    simulated_clock
)


def clock_generator(clk, delta_phase, audio_tick):

    audio_counter = Signal(intbv(0)[16:])

    @always(clk.posedge)
    def audio_sample_rate():
        if audio_counter >= DIVIDER - 1:
            audio_counter.next = 0
            audio_tick.next = True
        else:
            audio_counter.next = audio_counter + 1
            audio_tick.next = False

    @always(clk.posedge)
    def dphase_driver():
        delta_phase.next = DELTA_PHASE

    # TODO generate a 50Hz clock for parameter updates

    return (dphase_driver, audio_sample_rate)

def one_voice(audio_tick, pdata_in, pdata_out, _output):
    dphase = Signal(intbv(0)[24:])
    threshold = Signal(intbv(0)[N:])
    middle = Signal(intbv(0)[N:])

    controls = Signal(intbv(0)[8:])
    keydown = Signal(False)
    select = Signal(intbv(0)[2:])
    chorusing = Signal(False)

    envelope = Signal(intbv(0)[16:])
    attack = Signal(intbv(0)[4:])
    decay = Signal(intbv(0)[4:])
    sustain = Signal(intbv(0)[4:])
    _release = Signal(intbv(0)[4:])   # "release" is a reserved word in Verilog

    amplitude = Signal(intbv(0)[N:])

    a, b, c, d = get_nibbles(4)
    chain = DaisyChain(pdata_in)

    @always_comb
    def drive_pdata_out():
        pdata_out.next = d

    drivers = [
        drive_pdata_out,
        chain.param_24(dphase, a),
        chain.param_16(threshold, b, N),
        chain.param_8(controls, c),
        # chain.param_16(amplitude, d, N),
        bitfields(controls, (0, 1, keydown),
                            (1, 2, select),
                            (3, 1, chorusing)),
        chain.param_16(envelope, d),
        bitfields(envelope, (0, 4, attack),
                            (4, 4, decay),
                            (8, 4, sustain),
                            (12, 4, _release)),
        waveform_generator(audio_tick, dphase, chorusing, threshold, select, middle),
        adsr(audio_tick, attack, decay, sustain, _release, keydown, amplitude),
        vca(middle, amplitude, _output)
    ]

    return drivers


def fpga_synth(clk, param_data, param_clk, audio_req, audio_ack, dac_bit):
    # when I get to where I can pass in param data from the Arduino, switch to this.

    audio_tick = Signal(False)
    param_tick = Signal(False)
    areq_bit = Signal(False)
    _output = Signal(intbv(0)[N:])
    audio_counter = Signal(intbv(0)[16:])
    param_counter = Signal(intbv(1)[24:])

    @always(clk.posedge)
    def audio_sample_rate():
        if audio_counter >= DIVIDER - 1:
            audio_counter.next = 0
            audio_tick.next = True
        else:
            audio_counter.next = audio_counter + 1
            audio_tick.next = False

    @always(clk.posedge)
    def param_sample_rate():
        if param_counter >= PDIVIDER - 1:
            param_counter.next = 0
            param_tick.next = True
        else:
            param_counter.next = param_counter + 1
            param_tick.next = False

    @always(param_tick.posedge, audio_ack.posedge)
    def handshake():
        if audio_ack:
            areq_bit.next = False
        elif param_tick:
            areq_bit.next = True

    @always_comb
    def drive_audio_req():
        audio_req.next = areq_bit

    DaisyChain.set_up_clocks(param_clk, audio_req)
    a, b, c, d = get_nibbles(4)

    out1 = Signal(intbv(0)[N:])
    out2 = Signal(intbv(0)[N:])
    out3 = Signal(intbv(0)[N:])
    out4 = Signal(intbv(0)[N:])
    _output = Signal(intbv(0)[N:])

    @always_comb
    def drive_output():
        _output.next = (out1 + out2 + out3 + out4) >> 2

    dsig = delta_sigma_dac(clk, audio_tick, _output, dac_bit)

    drivers = [
        audio_sample_rate,
        param_sample_rate,
        handshake,
        drive_audio_req,
        one_voice(audio_tick, param_data, a, out1),
        one_voice(audio_tick, a, b, out2),
        one_voice(audio_tick, b, c, out3),
        one_voice(audio_tick, c, d, out4),
        drive_output,
        dsig
    ]

    return drivers


def real_fpga():

    # CHIP I/Os
    clk = Signal(False)
    param_data = Signal(intbv(0)[4:])
    param_clk = Signal(False)
    audio_req = Signal(False)
    audio_ack = Signal(False)
    dac_bit = Signal(True)

    toVerilog(fpga_synth, clk, param_data, param_clk, audio_req, audio_ack, dac_bit)
    # toVHDL(...)


def test_bench():

    # CHIP I/Os
    clk = Signal(False)
    param_data = Signal(intbv(0)[4:])
    param_clk = Signal(False)
    audio_req = Signal(False)
    audio_ack = Signal(False)
    dac_bit = Signal(True)

    simclk = simulated_clock(clk)
    fpga = fpga_synth(clk, param_data, param_clk, audio_req, audio_ack, dac_bit)

    a = int((1.e9 / MHZ) + 0.5)   # round to nearest integer
    b = int(0.5 * a)

    chorusing = 0
    select = 1    # two bits
    keydown = 1

    threshold = MASK
    controls = (chorusing << 3) | (select << 1) | keydown
    # amplitude = MASK
    envelope = 0x378F   # R=3, S=7, D=8, A=f

    param_bytes = (
        # amplitude & 0xFF,
        # (amplitude >> 8) & 0xFF,
        envelope & 0xFF,
        (envelope >> 8) & 0xFF,
        controls,
        threshold & 0xFF,
        (threshold >> 8) & 0xFF,
        DELTA_PHASE & 0xFF,
        (DELTA_PHASE >> 8) & 0xFF,
        (DELTA_PHASE >> 16) & 0xFF,
    )
    print map(hex, param_bytes)

    r = (daisy_chain_driver(param_bytes, param_data, audio_req, 10),
         param_clock_driver(param_bytes, param_clk, 10),
         fpga,
         simclk)
    return r

if __name__ == "__main__":
    if 'tb' in sys.argv[1:]:
        tb = traceSignals(test_bench)
        sim = Simulation(tb)
        sim.run()
    elif 'hdl' in sys.argv[1:]:
        real_fpga()
    else:
        raise Exception('broken code')
        suite = unittest.makeSuite(TestGrayCodeProperties)
        unittest.TextTestRunner(verbosity=2).run(suite)
