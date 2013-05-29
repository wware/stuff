# MyHDL code for a Moog-style synth done in DSP on a Xilinx FPGA

import unittest
import sys
from myhdl import Signal, delay, Simulation, always_comb, \
    instance, intbv, bin, toVerilog, toVHDL, always, now, traceSignals

from wavegen import waveform_generator, make_wavgen_ios
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
    N,
    WHOLE,
    HALF,
    MASK,
    SECOND,
    compute_delta_phase,
    unsigned_bus,
    signed_bus,
    signed_to_unsigned,
    unsigned_to_signed
)


A_ABOVE_MIDDLE_C = compute_delta_phase(440)
DELTA_PHASE = A_ABOVE_MIDDLE_C


def make_fpga_ios():
    fastclk = Signal(False)
    reset = Signal(False)
    param_data = unsigned_bus(4)
    param_clk = Signal(False)
    audio_req = Signal(False)
    audio_ack = Signal(False)
    dac_bit = Signal(True)
    return (fastclk, reset, param_data, param_clk, audio_req, audio_ack, dac_bit)


def fpga(fastclk, reset, param_data, param_clk, audio_req, audio_ack, dac_bit):

    aclk_counter = unsigned_bus(10)
    clk = Signal(False)

    # audio rate clock
    @always(fastclk.posedge, reset.posedge)
    def drive_audio_clock():
        if reset:
            aclk_counter.next = 0
            clk.next = True
        elif aclk_counter >= 800:
            aclk_counter.next = 0
            clk.next = True
        else:
            aclk_counter.next = aclk_counter + 1
            clk.next = False

    ignored, ignored2, select, threshold, delta_phase, wavgen_output = make_wavgen_ios()

    keydown = Signal(False)
    select = unsigned_bus(2)
    attack = unsigned_bus(4)
    decay = unsigned_bus(4)
    sustain = unsigned_bus(4)
    _release = unsigned_bus(4)
    amplitude = unsigned_bus(N)
    _output = signed_bus(N)

    # more bits than we really need, 18 bits would give 6.5536 seconds
    input_driver_count = unsigned_bus(24)

    @always(clk.posedge, reset.posedge)
    def drive_inputs():
        attack.next = 3
        decay.next = 5
        sustain.next = 8
        _release.next = 0
        delta_phase.next = DELTA_PHASE
        select.next = 1
        threshold.next = HALF
        keydown.next = 0
        if reset:
            keydown.next = 0
            input_driver_count.next = 0
        elif input_driver_count >= 5 * SECOND:
            keydown.next = 0
            input_driver_count.next = 0
        elif input_driver_count < 2 * SECOND:
            keydown.next = 1
            input_driver_count.next = input_driver_count + 1
        else:
            keydown.next = 0
            input_driver_count.next = input_driver_count + 1

    drivers = [
        drive_audio_clock,
        drive_inputs,
        waveform_generator(clk, reset, select, threshold, delta_phase, wavgen_output),
        # adsr(clk, reset, keydown, attack, decay, sustain, _release, amplitude),
        # vca(fastclk, reset, wavgen_output, amplitude, _output),
        delta_sigma_dac(fastclk, clk, reset, wavgen_output, dac_bit),
    ]
    return drivers

def simulate():

    # don't make the simulation take all day
    global SECOND, DELTA_PHASE
    SECOND = 200
    DELTA_PHASE = compute_delta_phase(1000)

    fastclk, reset, param_data, param_clk, audio_req, audio_ack, dac_bit = make_fpga_ios()

    @instance
    def bench():
        fastclk.next = 0
        reset.next = 0
        param_data.next = 0
        param_clk.next = 0
        audio_req.next = 0
        audio_ack.next = 0
        yield delay(1)
        reset.next = 1
        yield delay(1)
        reset.next = 0
        # for i in range(8 * SECOND * 32000000 / 40000):
        for i in range(SECOND * 32000000 / 40000):
            yield delay(1)
            fastclk.next = 1
            yield delay(1)
            fastclk.next = 0

    stuff = [
        bench,
        fpga(fastclk, reset, param_data, param_clk, audio_req, audio_ack, dac_bit)
    ]
    return stuff


class TestFpga(unittest.TestCase):
    # TODO write tests
    pass


if __name__ == '__main__':
    if 'hdl' in sys.argv[1:]:
        fastclk, reset, param_data, param_clk, audio_req, audio_ack, dac_bit = make_fpga_ios()
        toVerilog(fpga, fastclk, reset, param_data, param_clk, audio_req, audio_ack, dac_bit)
    elif 'sim' in sys.argv[1:]:
        Simulation(traceSignals(simulate)).run()
    else:
        suite = unittest.TestLoader().loadTestsFromTestCase(TestFpga)
        unittest.TextTestRunner(verbosity=2).run(suite)
