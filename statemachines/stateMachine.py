"""
State machines should help to switch back and forth between simulated
environments and real ones.
"""

import os, pprint, time, types

TIME_GRANULARITY = 0.01

SIMULATOR = False

try:
    SIMULATOR = os.environ['SIMULATOR'].lower() in ('1', 'yes', 'true')
except KeyError:
    pass

###################################################

lc = 'abcdefghijklmnopqrstuvwxyz'
uc = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

def dotString(str):
    if len(str) >= 15:
        # try to find a space near the middle
        n = len(str) / 2
        for i in range(len(str)/4):
            if str[n-i] == ' ':
                return ' ' + str[:n-i] + ' \\n ' + str[(n-i)+1:] + ' '
            if str[n+i] == ' ':
                return ' ' + str[:n+i] + ' \\n ' + str[(n+i)+1:] + ' '
            if str[n-i] in lc and str[(n-i)+1] in uc:
                return ' ' + str[:(n-i)+1] + ' \\n ' + str[(n-i)+1:] + ' '
            if str[n+i] in lc and str[(n+i)+1] in uc:
                return ' ' + str[:n+i+1] + ' \\n ' + str[n+i+1:] + ' '
    return ' ' + str + ' '

class StateMachine:
    """
    State machines are finished when their _state is set to None.
    Then you can safetly garbage-collect them.
    """

    class State:
        def __init__(self, name, action=None):
            self.name = name
            self.action = action
            # If the action is not None, then it should run when you
            # enter the state. The action for the initial state should
            # be run in the constructor of the state machine.
            self.dotName = name.replace(' ', '_')

    class Transition:
        """
        This represents a state machine's transition from one state to
        the next. From a state there can be a list of possible transitions
        to other states, and the first Transition whose test passes is
        the one that decides where the state machine will go next.

        'nextstate' is the state. It must be an integer.

        'test' is a no-arg function returning a boolean. If test()
        returns True then make the transition. If test is None then
        the transition is unconditional. IMPORTANT! The test function
        must have no side-effects, because we don't know when it will
        be run or how many times.

        'action' is a no-arg function whose return value is ignored. It
        will be run only ONCE, the first time the test passes, right
        before moving to the next state. The action SHOULD have
        side-effects since if it doesn't, it will have no effect at all.

        'delay' is used during simulation. It can either be a number (int
        or float) or a function that returns a number. If present, it
        will delay the arrival at the new state until simulation time
        reaches or passes that number. In the meanwhile the state machine
        will be an idle state doing nothing.
        """
        def __init__(self, nextstate, test=None, action=None, delay=None):
            assert nextstate is None or isinstance(nextstate, StateMachine.State)
            self.nextstate = nextstate
            self.test = test
            self.action = action
            self.delay = delay

        def repr(self, statenames=None):
            r = '<Transition'
            if statenames is None:
                r += ' ' + repr(self.nextstate)
            elif self.nextstate is None:
                r += ' None'
            else:
                r += ' "' + self.nextstate.name + '"'
            if self.test is not None:
                r += ' test=' + self.test.func_name
            if self.action is not None:
                r += ' action=' + self.action.func_name
            if type(self.delay) in (types.IntType, types.FloatType):
                r += ' delay=' + str(self.delay)
            elif callable(self.delay):
                r += ' delay=' + self.delay.func_name
            else:
                assert self.delay is None
            return r + '>'

        def __repr__(self):
            return self.repr()

        def dot(self, statemachine, oldstate):
            if self.nextstate is None:
                return ''
            r = oldstate.dotName + ' -> ' + self.nextstate.dotName
            label = ''
            if self.test is not None:
                label += self.test.func_name
            if type(self.delay) in (types.IntType, types.FloatType):
                label += '(' + str(self.delay) + ')'
            elif callable(self.delay):
                label += '(' + self.delay.func_name + ')'
            if len(label) > 0:
                r += ' [label="' + dotString(label) + '",fontname="Verdana"]'
            else:
                r += ' [label="    ",fontname="Verdana"]'
            return r

        def go(self, statemachine):
            if self.test is None or self.test(self=statemachine):
                if statemachine._debug:
                    print statemachine, 'transitioning via', \
                        self.repr(statemachine.states)
                if self.action is not None:
                    self.action(self=statemachine)
                delay = self.delay
                if callable(delay):
                    delay = delay(self=statemachine)
                if delay is None:
                    statemachine._state = self.nextstate
                    statemachine._next_time = None
                else:
                    statemachine._state = statemachine._wait_for_changeState
                    def changeState(sm=statemachine,
                                    st=self.nextstate):
                        sm._state = st
                    time = getRunner().time()
                    statemachine.postEvent(time + delay, changeState)
                    statemachine._next_time = delay
                return True
            else:
                #if statemachine._debug:
                #    print statemachine, 'ignoring', \
                #        self.repr(statemachine.states)
                return False

    _wait_for_changeState = State("idle")

    def __init__(self):
        self._state = self.initial_state
        self._debug = False
        self._next_time = None
        self._no_transitions_taken = False

    def __repr__(self):
        r = '<' + self.__class__.__name__
        id = self.id()
        if len(id) > 0:
            r += ' ' + id
        #if self.isWaiting():
        #    r += ' (waiting)'
        if self._state is None:
            r += ' None'
        else:
            r += ' "' + self._state.name + '"'
        return r + '>'

    def id(self):
        return hex(id(self))

    def step(self):
        self._no_transitions_taken = False
        if self._state == self._wait_for_changeState:
            return
        if self._state is not None:
            for transition in self.transitions[self._state]:
                if transition.go(self):
                    return
        self._no_transitions_taken = True

    def isWaiting(self):
        return self._state == self._wait_for_changeState \
            or self._no_transitions_taken

    def postEvent(self, time, statechange):
        getRunner().addEvent(time, statechange)

    def dot(self, filename=None):
        """Generate DOT notation for graphviz"""
        r = 'digraph G {\n'
        # states, or in graph terms, nodes/vertices
        for i in range(len(self.states)):
            st = self.states[i]
            r += '  ' + st.dotName + ' [label="%s",fontname="Verdana"]\n' \
                % dotString(self.states[i].name)
        # edges next
        for oldstate in self.transitions.keys():
            tlist = self.transitions[oldstate]
            if len(tlist) == 0:
                r += '  ' + oldstate.dotName + ' -> ' + oldstate.dotName
            else:
                for tr in tlist:
                    r += '  ' + tr.dot(self, oldstate) + '\n'
        r += '}'
        if filename is not None:
            outf = open(filename, 'w')
            outf.write(r + '\n')
            outf.close()
        else:
            return r

########################################################

"""
A Runner is an execution environment for a bunch of state machines.
It provides an API (postEvent) allowing state machines to post
events in the future by putting them in an event queue, which is
kept sorted chronologically so it's easy to pull out the soonest
event.

There will be two kinds of Runners: real-time Runners and simulation
Runners.

A real-time Runner executes events in real time. This is done in the
same loop that steps all the state machines.

A simulation Runner maintains a simulation clock which skips over the
time intervals. If all state machines are idle then it will grab the
soonest event, set the simulation time to the time of that event, then
execute that event and all events with that same time.
"""

class Runner:
    # let's make this the real-time version

    def __init__(self):
        self.machines = [ ]
        self.eventQueue = [ ]
        self._debug = False
        self.zeroTime()

    def printStatus(self):
        def pair(tf):
            t, f = tf
            if f.__doc__ is not None:
                return (t, f.__doc__)
            else:
                return (t, f)
        if self._debug:
            print
            print 'time', self.time()
            print 'MACHINES'
            pprint.pprint(self.machines)
            print 'EVENT QUEUE'
            pprint.pprint(map(pair, self.eventQueue))

    def addMachine(self, machine):
        self.machines.append(machine)

    def removeMachine(self, machine):
        self.machines.remove(machine)

    def addEvent(self, time, thunk):
        assert callable(thunk)
        def same(tf, time=time, thunk=thunk):
            t, f = tf
            f1, f2 = f.func_code, thunk.func_code
            return t == time and \
                f1.co_name == f2.co_name and \
                f1.co_filename == f2.co_filename and \
                f1.co_firstlineno == f2.co_firstlineno
        self.eventQueue.append((time, thunk))
        self.eventQueue.sort()

    def zeroTime(self):
        self._start_time = time.time()

    def time(self):
        # real time version
        return time.time() - self._start_time

    def waitUntil(self, t):
        time.sleep(t - self.time())

    def run(self, maxtime=1.0e100):
        def allWaiting(machines=self.machines):
            for m in machines:
                # if m is still in the game and not waiting
                if not m.isWaiting() \
                        and m._state is not None:
                    return False
            return True
        while self.time() < maxtime:

            # self.printStatus()

            [m.step() for m in self.machines]

            for m in filter(lambda m: m._state is None,
                            self.machines):
                self.machines.remove(m)

            now = self.time()
            if allWaiting():
                if len(self.eventQueue) > 0:
                    self.waitUntil(now + TIME_GRANULARITY)
                    while len(self.eventQueue) > 0 \
                            and self.eventQueue[0][0] <= now:
                        ignore, event = self.eventQueue.pop(0)
                        event()
                elif SIMULATOR:
                    if len(self.eventQueue) == 0:
                        self.printStatus()
                        raise Exception('Everybody is waiting but'
                                        + ' no events on the way')
            else:
                # don't hog the processor
                self.waitUntil(now + TIME_GRANULARITY)

########################################################

class Simulator(Runner):

    def zeroTime(self):
        self._time = 0.0

    def setTime(self, t):
        self._time = t

    def time(self):
        return self._time

    def waitUntil(self, t):
        self._time = t

########################################################

_runner_instance = None

def getRunner(simulate=False):
    global _runner_instance
    if _runner_instance is None:
        if SIMULATOR:
            _runner_instance = Simulator()
        else:
            _runner_instance = Runner()
    return _runner_instance

def T():
    return getRunner().time()
