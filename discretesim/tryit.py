import sys
import random

# use simpy or do it myself?
# http://simpy.sourceforge.net/examples/a%20cellphone%20example.htm
# I'll learn it one of these days, but doing it myself is more fun.

class HaltSimulation(Exception):
    pass

class DiscreteEvent:

    def __init__(self, t0):
        self._time = t0

    def time(self):
        return 0.0

    def call(self):
        assert False, "overload me, I'm an abstract method"


class ClockEvent(DiscreteEvent):

    def __init__(self, sim, t0, period):
        self.sim = sim
        self._time = t0
        self.period = period

    def time(self):
        return self._time

    def call(self):
        self.tick()
        nextguy = apply(self.__class__,
                        (self.sim,
                         self._time + self.period, self.period))
        self.sim.add(nextguy)

    def tick(self):
        assert False, "overload me, I'm an abstract method"


class Simulator:
    """
    >>> class TickTick(ClockEvent):
    ...     class TockEvent(DiscreteEvent):
    ...         def call(self):
    ...             print self._time, 'TOCK'
    ...     def tick(self):
    ...         self.sim.add(self.TockEvent(self._time + 0.1))
    ...         print self._time, 'TICK'
    >>> sim = Simulator(timelimit=1.75)
    >>> evt = TickTick(sim, 0.0, 0.5)
    >>> sim.add(evt)
    >>> sim.go()
    0.0 TICK
    0.1 TOCK
    0.5 TICK
    0.6 TOCK
    1.0 TICK
    1.1 TOCK
    1.5 TICK
    1.6 TOCK
    """
    def __init__(self, timelimit=1.0e20):
        self.queue = [ ]
        self.timelimit = timelimit

    def add(self, evt):
        self.queue.append(evt)
        self.queue.sort(lambda x, y: cmp(x.time(), y.time()))

    def go(self):
        t = 0.0
        while len(self.queue) > 0:
            event = self.queue.pop(0)
            t = event.time()
            if t >= self.timelimit:
                return
            try:
                # self.beforeCall(t)
                event.call()
                # self.afterCall(t)
            except HaltSimulation:
                return


if __name__ == '__main__':
    import doctest
    doctest.testmod()
