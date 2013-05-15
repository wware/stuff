from myhdl import Signal, delay, Simulation, always_comb, \
    instance, intbv, bin, toVerilog, toVHDL, always, now, traceSignals
from config import (
    PHASEWIDTH,
    N,
    CDIFF1,
    CDIFF2,
)

# Single generator of ramp wave, triangle wave, or PW-modulated square wave
def single_waveform(audio_tick, select, threshold, delta_phase, _output):

    phase_counter = Signal(intbv(0)[24:])

    @always(audio_tick.posedge)
    def waveforms():
        if phase_counter + delta_phase >= (1 << PHASEWIDTH):
            phase_counter.next = phase_counter + delta_phase - (1 << PHASEWIDTH)
        else:
            phase_counter.next = phase_counter + delta_phase

        if select == 0:     # ramp
            _output.next = phase_counter >> 10
        elif select == 1:   # triangle
            if phase_counter < (1 << (PHASEWIDTH - 1)):
                _output.next = phase_counter >> 9
            else:
                _output.next = ((1 << PHASEWIDTH) - 1 - phase_counter) >> 9
        elif select == 2:   # square/pulse-width
            if phase_counter > (threshold << (PHASEWIDTH - N)):
                _output.next = (1 << N) - 1
            else:
                _output.next = 0
        # in waveform_generator,  3 is noise

    return waveforms


def waveform_generator(audio_tick, delta_phase, chorusing, threshold, select, _output):

    noise_register = Signal(intbv(0)[16:])

    dphase1 = Signal(intbv(0)[PHASEWIDTH:])
    dphase2 = Signal(intbv(0)[PHASEWIDTH:])
    dphase3 = Signal(intbv(0)[PHASEWIDTH:])

    output0 = Signal(intbv(0)[N:])
    output1 = Signal(intbv(0)[N:])
    output2 = Signal(intbv(0)[N:])
    output3 = Signal(intbv(0)[N:])

    @always(audio_tick.posedge)
    def noise_source():
        if noise_register == 0:
            noise_register.next = 123
        elif (noise_register ^ (noise_register >> 2) ^
                (noise_register >> 3) ^ (noise_register >> 5)) & 1:
            noise_register.next = (1 << 15) + (noise_register >> 1)
        else:
            noise_register.next = (noise_register >> 1)

    @always_comb
    def output_selector():
        dphase1.next = delta_phase - CDIFF1
        dphase2.next = delta_phase + CDIFF1
        dphase3.next = delta_phase + CDIFF2
        if select == 3:
            _output.next = noise_register >> 2
        elif chorusing:
            _output.next = (output0 + output1 + output2 + output3) >> 2
        else:
            _output.next = output0

    w0 = single_waveform(audio_tick, select, threshold, delta_phase, output0)
    w1 = single_waveform(audio_tick, select, threshold, dphase1, output1)
    w2 = single_waveform(audio_tick, select, threshold, dphase2, output2)
    w3 = single_waveform(audio_tick, select, threshold, dphase3, output3)

    return (w0, w1, w2, w3, output_selector, noise_source)
