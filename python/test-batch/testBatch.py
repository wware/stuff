#!/usr/bin/python

"""
Extend Python's unittest module to cover situations where the same test
needs to be run on a large number of subjects, and you want an individual
success or failure for each one.
"""

import unittest
import sys
import traceback

_index = 0
_testname = None

class TestBatch(unittest.TestCase):

    # Test batch classes should define a class method called subjects()
    # which returns a list of the subjects to be tested, and a oneTest()
    # instance method which should be applied to each one. These will be
    # used to populate the class with a test method for each subject.

    @classmethod
    def prepare(cls):
        global _index
        import new
        for subject in cls.subjects():
            def testMethod(self, subject=subject):
                self.oneTest(subject)
            setattr(cls, 'test_%06d' % _index,
                    new.instancemethod(testMethod, None, cls))
            _index += 1


def isConcreteTestBatch(obj):
    def ancestorIsTestBatch(x):
        try:
            if TestBatch in x.__bases__:
                return True
        except AttributeError:
            return False

        for baseClass in x.__bases__:
            if ancestorIsTestBatch(baseClass):
                return True
        return False

    if obj is None or not ancestorIsTestBatch(obj):
        return False
    if not hasattr(obj, 'subjects'):
        return False
    if not hasattr(obj, 'oneTest'):
        return False
    return True


# diff between Python 2.6 and Python 2.7
if not hasattr(unittest, "TextTestResult"):
    unittest.TextTestResult = unittest._TextTestResult


# One-line error reporting for each subject in a batch
class TerseTestResult(unittest.TextTestResult):
    def printErrorList(self, flavorIgnored, errors):
        for test, err in errors:
            err = err.split('\n')[-2]
            prefix = 'AssertionError: '
            if err.startswith(prefix):
                err = err[len(prefix):]
            self.stream.writeln(err)


class TerseTestRunner(unittest.TextTestRunner):

    resultclass = TerseTestResult

    def run(self, test):
        result = self._makeResult()
        # diffs between Python 2.6 and Python 2.7
        if hasattr(unittest, 'registerResult'):
            unittest.registerResult(result)
        if hasattr(result, 'failfast'):
            result.failfast = self.failfast
        if hasattr(result, 'buffer'):
            result.buffer = self.buffer
        test(result)
        self.stream.writeln((_testname + ": %d tests, %d failures, %d errors") %
                            (result.testsRun, len(result.failures), len(result.errors)))
        result.printErrors()
        return result


def main(globals, args=[], verbose=False,
         stream=None, runnerclass=None):
    #
    # typical usage:
    #
    #     import testBatch
    #     testBatch.main(globals(), sys.argv[1:])
    #
    if len(args) > 0:
        testcases = filter(
            lambda g, args=args: hasattr(g, '__name__') and \
                g.__name__ in args,
            globals.values())
    else:
        testcases = filter(
            lambda g: isConcreteTestBatch(g),
            globals.values())

    if stream is None:
        stream = sys.stderr

    if runnerclass is None:
        runnerclass = TerseTestRunner

    verbosity = 0
    if verbose:
        runnerclass = unittest.TextTestRunner
        verbosity = 2

    for index, testcase in enumerate(testcases):
        global _testname
        _testname = testcase.__name__
        testcase.prepare()
        runnerclass(verbosity=verbosity,
                    stream=stream).run(
            unittest.TestLoader().
            loadTestsFromTestCase(testcase))
        if index < len(testcases) - 1:
            stream.write("\n")


if __name__ == '__main__':

    class ExampleBatch(TestBatch):

        """
        For the integers up to 10000, divisibility by 12
        implies divisibility by 3.
        """

        @classmethod
        def subjects(cls):
            """
            These are the subjects we are going to test.
            """
            return range(10000)

        def oneTest(self, subject):
            """
            This is the test that will be run on each subject.
            """
            div12 = (subject % 12) == 0
            div3 = (subject % 3) == 0
            # the implication fails if the "if" clause
            # is true while the "then" clause is false
            badImply = div12 and not div3
            # if it fails, assign blame
            self.assertFalse(badImply, subject)

    main(globals())
