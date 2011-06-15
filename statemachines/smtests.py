# Unit tests for state machines and simulator

from stateMachine import StateMachine, getRunner
import unittest
import random
import sys
random.seed(12)

import stateMachine
stateMachine._runner_instance = stateMachine.Simulator()

# This is useful for looking at time sloppiness, but not
# for automated testing.
#
# stateMachine._runner_instance = stateMachine.Runner()

Edge = StateMachine.Transition
State = StateMachine.State

threshold = 0.35

class Example1(StateMachine):

    results = [ ]

    s0 = State('state 0')
    s1 = State('state 1')
    s2 = State('state 2')
    s3 = State('state 3')

    initial_state = s0

    states = ( s0, s1, s2, s3 )

    def test1(self):
        return random.random() < threshold
    def action1(self):
        self.results.append(1)

    def test2(self):
        return random.random() < threshold
    def action2(self):
        self.results.append(2)

    def test3(self):
        return random.random() < threshold
    def action3(self):
        self.results.append(3)

    def postEvent(self, time, statechange):
        # just do the statechange immediately
        statechange()

    transitions = {

        s0: [Edge(s1, test1, action1),
             Edge(s2)],

        s1: [Edge(s0, test2),
             Edge(s2, test3, action3)],

        s2: [Edge(s1)],

        s3: [ ]     # no transitions, stay on state 3 forever

        }

# Graphviz can make pretty diagrams of state machines
def useGraphviz(self):
    foo.dot('foo.dot')
    import os
    os.system('dot -Tps foo.dot -o foo.ps')
    os.system('evince foo.ps')


trigger = False

class Example2(StateMachine):

    def __init__(self):
        StateMachine.__init__(self)
        self._trigger = False

    def id(self):
        return 'hello'

    # tests are functional, no side-effects
    def getTrigger(self):
        return self._trigger

    # actions have ONLY side-effects
    def setTrigger(self):
        self._trigger = True
    def clearTrigger(self):
        self._trigger = False

    s0 = State('zero')
    s1 = State('one')
    s2 = State('two')

    initial_state = s0

    states = ( s0, s1, s2 )

    transitions = {

        s0: [ Edge(s1) ],

        s1: [ Edge(s2) ],

        s2: [ Edge(s0,
                  test=getTrigger,
                  action=clearTrigger) ]

        }


class Example3(StateMachine):

    def __init__(self, incr):
        StateMachine.__init__(self)
        self.results = [ ]
        self._incr = incr

    allResults = [ ]

    def getIncr(self):
        return self._incr

    s0 = State('zero')
    s1 = State('one')
    s2 = State('two')

    initial_state = s0

    states = ( s0, s1, s2 )

    def record_result(self, x):
        x = str(self.sim.time()) + ' ' + x.name
        self.results.append(x)
        self.allResults.append(x)

    transitions = {

        s0: [ Edge(s1,
                   action=lambda self: self.record_result(self.s0)) ],

        s1: [ Edge(s2,
                   action=lambda self: self.record_result(self.s1)) ],

        s2: [ Edge(s0,
                   action=lambda self: self.record_result(self.s2),
                   delay=getIncr) ],

        }



class StateMachineTests(unittest.TestCase):

    def setUp(self):
        sim = getRunner()
        sim.zeroTime()
        Example3.allResults = [ ]
        self.e3a = Example3(1)
        self.e3b = Example3(1.63)
        self.e3a.sim = sim
        self.e3b.sim = sim
        sim.addMachine(self.e3a)
        sim.addMachine(self.e3b)
        sim.run(maxtime=8)
        sim.removeMachine(self.e3a)
        sim.removeMachine(self.e3b)

    def test_1(self):
        Example1.results = [ ]
        foo = Example1()
        for i in range(12):
            foo.step()
        assert Example1.results == [ 1, 3, 3, 3 ]

    def test_2(self):
        ex1 = Example2()
        results = [ ]
        for i in range(5):
            self.assertFalse(ex1.getTrigger())
            results.append(repr(ex1))
            ex1.step()
        ex1.setTrigger()
        for i in range(6):
            results.append(repr(ex1))
            ex1.step()
            self.assertFalse(ex1.getTrigger())
        self.assertEqual(results,
                         ['<Example2 hello "zero">',
                          '<Example2 hello "one">',
                          '<Example2 hello "two">',
                          '<Example2 hello "two">',
                          '<Example2 hello "two">',
                          '<Example2 hello "two">',
                          '<Example2 hello "zero">',
                          '<Example2 hello "one">',
                          '<Example2 hello "two">',
                          '<Example2 hello "two">',
                          '<Example2 hello "two">'])

    def test_3(self):
        self.assertEqual(self.e3a.results,
                         ['0.0 zero',
                          '0.01 one',
                          '0.02 two',
                          '1.03 zero',
                          '1.04 one',
                          '1.05 two',
                          '2.07 zero',
                          '2.08 one',
                          '2.09 two',
                          '3.11 zero',
                          '3.12 one',
                          '3.13 two',
                          '4.15 zero',
                          '4.16 one',
                          '4.17 two',
                          '5.19 zero',
                          '5.2 one',
                          '5.21 two',
                          '6.23 zero',
                          '6.24 one',
                          '6.25 two',
                          '7.27 zero',
                          '7.28 one',
                          '7.29 two']
                          )

    def test_4(self):
        self.assertEqual(self.e3b.results,
                         ['0.0 zero',
                          '0.01 one',
                          '0.02 two',
                          '1.66 zero',
                          '1.67 one',
                          '1.68 two',
                          '3.33 zero',
                          '3.34 one',
                          '3.35 two',
                          '5.0 zero',
                          '5.01 one',
                          '5.02 two',
                          '6.67 zero',
                          '6.68 one',
                          '6.69 two'])

    def test_5(self):
        self.assertEqual(Example3.allResults,
                         ['0.0 zero',
                          '0.0 zero',
                          '0.01 one',
                          '0.01 one',
                          '0.02 two',
                          '0.02 two',
                          '1.03 zero',
                          '1.04 one',
                          '1.05 two',
                          '1.66 zero',
                          '1.67 one',
                          '1.68 two',
                          '2.07 zero',
                          '2.08 one',
                          '2.09 two',
                          '3.11 zero',
                          '3.12 one',
                          '3.13 two',
                          '3.33 zero',
                          '3.34 one',
                          '3.35 two',
                          '4.15 zero',
                          '4.16 one',
                          '4.17 two',
                          '5.0 zero',
                          '5.01 one',
                          '5.02 two',
                          '5.19 zero',
                          '5.2 one',
                          '5.21 two',
                          '6.23 zero',
                          '6.24 one',
                          '6.25 two',
                          '6.67 zero',
                          '6.68 one',
                          '6.69 two',
                          '7.27 zero',
                          '7.28 one',
                          '7.29 two'])


if __name__ == '__main__':
    if 'dot' in sys.argv[1:]:
        Example1().dot('ex1.dot')
        Example2().dot('ex2.dot')
        Example3(1).dot('ex3.dot')
    else:
        unittest.main()
