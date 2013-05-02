import math

from myhdl import Signal, delay, Simulation, always_comb, \
    instance, intbv, bin, toVerilog, toVHDL, always, now, traceSignals
from config import (
    MHZ,
    N,
)

DT = 1./MHZ
R = 1000
C = 0.01e-6
ALPHA = math.exp(-DT / (R * C))
A = int(round((1. - ALPHA) * (1 << 18)))
B = int(round(ALPHA * (1 << 18)))


def interpolator(clk, audio_tick, input_data, interp_out):

    rm_counter = Signal(intbv(0)[5:])
    rm_tick = Signal(False)
    delay_1 = Signal(intbv(0)[N:])
    interp_step = Signal(intbv(0)[(N+9):])
    interp_data = Signal(intbv(0)[(N+9):])
    direction = Signal(False)

    @always(clk.posedge)
    def do_stuff():
        if (rm_counter == 0 or rm_counter == 3 or rm_counter == 6 or rm_counter == 9
                or rm_counter == 11 or rm_counter == 14 or rm_counter == 17
                or rm_counter == 20 or rm_counter == 23):
            rm_tick.next = False
        else:
            rm_tick.next = True
        if rm_counter >= 24:
            rm_counter.next = 0
        else:
            rm_counter.next = rm_counter + 1
        if audio_tick:
            delay_1.next = input_data
            interp_data.next = delay_1 << 9
            if input_data > delay_1:
                direction.next = True
                interp_step.next = input_data - delay_1
            else:
                direction.next = False
                interp_step.next = delay_1 - input_data
        elif rm_tick:
            if direction:
                interp_data.next = interp_data + interp_step
            else:
                interp_data.next = interp_data - interp_step

    @always_comb
    def rightshift_for_output():
        interp_out.next = interp_data >> 9

    return (do_stuff, rightshift_for_output)


def delta_sigma_dac(clk, audio_tick, input_data, dac_bit):

    interp_result = Signal(intbv(0)[N:])
    vc_estimate = Signal(intbv(0)[18:])
    sum_of_products = Signal(intbv(0)[36:])

    _interp = interpolator(clk, audio_tick, input_data, interp_result)

    # I think interpolation will be a huge help with anti-aliasing.
    @always(clk.posedge)
    def do_stuff():
        dac_bit.next = interp_result > (sum_of_products >> 18)
        vc_estimate.next = sum_of_products >> 18

    # @always(clk.posedge)
    # def do_stuff():
    #     dac_bit.next = input_data > vc_estimate
    #     vc_estimate.next = sum_of_products >> 18

    @always_comb
    def multiply():
        if dac_bit:
            sum_of_products.next = (A << N) + (B * vc_estimate)
        else:
            sum_of_products.next = B * vc_estimate

    return (_interp, do_stuff, multiply)
