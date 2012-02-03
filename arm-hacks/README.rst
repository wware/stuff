ARM microcontrollers
====================

`List of ARM cores`_

.. _`List of ARM cores`: http://en.wikipedia.org/wiki/List_of_ARM_microprocessor_cores

In the past I've played with the ARM7TDMI cores, particularly the SAM7 family
from Atmel. More recently I'm interested in the more advanced Cortex-M3 core,
with one of the more popular implementations being the STM32_ family from ST
Microelectronics, used on the Leaflabs Maple board and the STM32VL Discovery_
board.

.. _STM32: http://en.wikipedia.org/wiki/STM32
.. _Discovery: http://en.wikipedia.org/wiki/STM32#Discovery_kits

As a general rule, each manufacturer designs their own peripherals that they
connect to the core. The SAM7 peripherals typically have one 32-bit register
that sets bits and another that clears bits.

The Discovery boards have a port of an RTOS called `ChibiOS/RT`_.

.. _`ChibiOS/RT`: http://en.wikipedia.org/wiki/ChibiOS/RT

It provides preemptive multithreading, 128 priority levels, a scheduler,
timers, semaphores, mutexes, queues, etc. So that would be an excellent basis
for tinkering with my idea for a Javascript-inspired microcontroller system,
where there's a very responsive main thread that spawns worker threads for
anything non-responsive.

I think that should be considered more a style of programming than a system
in its own right. Like the way there are design patterns but they don't
specify implementations, just ways of thinking.
