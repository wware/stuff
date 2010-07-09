import random

DEBUGLEVEL = 0

# I'd like to force the map jobs to be more fully independent. I think
# one way to do this, which would also help to prep code to be
# hadoop-ready, would be to make Map and Reduce external (i.e. scripts
# or executables) rather than methods in a class that extends this
# class. My reading of the hadoop docs makes me think that the data
# going back and forth needs to be text, not binary, and needs to
# specify a separator (e.g. a comma). So I should follow that
# convention and make this more like a hadoop ref impl. Then when I
# want to run stuff on a real hadoop cluster (like Amazon's) I'll
# really be prepared to do that.

class MapReduce:

    # These are things to be defined by the user. If this reference
    # implementation is done correctly, then any code that works with
    # this will also work on a real map-reduce system distributed over
    # a big cluster of machines.

    def InputReader(self, emit):
        """Read the input data, wherever it might come from, and emit a bunch
        of (k1,v1) key-value pairs."""
        raise Exception("overload this abstract method")
    def Map(self, k1, v1, emit):
        """Given a (k1,v1) pair, do some processing that is independent of all
        other (k1,v1) pairs, and emit some (k2,v2) pairs."""
        raise Exception("overload this abstract method")
    def Reduce(self, k2, v2iter, emit):
        raise Exception("overload this abstract method")
    def OutputWriter(self, queues):
        raise Exception("overload this abstract method")

    def Run(self):

        """Reference implementation"""

        class Iterator:
            # This class is a wrapper for a python list that provides a
            # read-only iterator API.
            def __init__(self):
                self.L = [ ]
            # Use this to populate the iterator's data, then get
            # rid of this to make the iterator read-only.
            def add(self, x):
                self.L.append(x)
            # The next two methods are Python's standard iterator API.
            def __iter__(self):
                return self
            def next(self):
                try:
                    r, self.L = self.L[0], self.L[1:]
                    return r
                except IndexError:
                    raise StopIteration

        # Run a Map job each time the InputReader produces a (k1,v1) pair.
        intermediateQueues = { }
        def emitIntermediate(k2, v2, queues=intermediateQueues):
            if not queues.has_key(k2):
                queues[k2] = Iterator()
            queues[k2].add(v2)
        self.InputReader(lambda k1,v1: self.Map(k1,v1,emitIntermediate))
        for it in intermediateQueues.values():
            it.add = None   # make it read-only

        # Run a Reduce job for each k2 key. This produces a collection
        # of output iterators which will feed the output writer.
        outputQueues = { }
        def emitOutput(k2, v2, queues=outputQueues):
            if not queues.has_key(k2):
                queues[k2] = Iterator()
            queues[k2].add(v2)
        for k2, v2iter in intermediateQueues.items():
            # The reduce jobs for particular k2 keys can run in parallel
            # but reduce jobs cannot be further parallelized.
            self.Reduce(k2, v2iter, emitOutput)
        for it in outputQueues.values():
            it.add = None   # make it read-only

        # Do whatever needs to be done with output.
        self.OutputWriter(outputQueues)

    def RunWithRandomizedMaps(self):

        """This second Run function tests whether the map jobs are independent
        of one another, by running them in random order. There could
        still be ways they interact that would cause them to fail on a
        cluster where map jobs were really forced to be totally
        independent, but it's a useful and inexpensive test given that
        things are running in a single thread on a single CPU."""

        # Run the input reader to get a set of (k1,v1) pairs, each of
        # which will be the input to a map job.

        lst = [ ]   # (k1,v1) pairs go here, with index

        # The index allows us to perform the map jobs in random order
        # later, but still be able to put the (k2,v2lst) info back
        # into correct order.

        def emit(k1, v1, index=[0]):
            lst.append((index[0], k1, v1))
            index[0] += 1
        self.InputReader(emit)
        numMapJobs = len(lst)
        if DEBUGLEVEL > 0:
            print numMapJobs, "map jobs"

        # This iterator class, which applies to a particular k2 key,
        # maintains separate output lists for each map job, and uses
        # the index to read them back in the proper order when the
        # reduce job iterates over this. When this thing runs on a
        # cluster, these separate output lists will reside on
        # different machines and this knitting-together will become
        # more complicated.

        class Iterator:
            def __init__(self):
                self._lst = [ ]
                for i in range(numMapJobs):
                    self._lst.append([ ])
                self._i = self._j = 0
            def add(self, n, x):
                self._lst[n].append(x)
            def __iter__(self):
                return self
            def next(self):
                while True:
                    i, j, lst = self._i, self._j, self._lst
                    if i == len(lst):
                        raise StopIteration
                    elif j == len(lst[i]):
                        self._i, self._j = i + 1, 0
                    else:
                        r = lst[i][j]
                        self._i, self._j = i, j + 1
                        return r

        # Randomize the order of (k1,v1) pairs so we do the map
        # operations in random order.

        lst2 = [ ]
        while lst:
            x = random.choice(lst)
            lst.remove(x)
            lst2.append(x)
        v2iters = { }
        for n, k1, v1 in lst2:
            def emit(k2, v2, n=n):
                if not v2iters.has_key(k2):
                    v2iters[k2] = Iterator()
                v2iters[k2].add(n, v2)
            self.Map(k1, v1, emit)
        for it in v2iters.values():
            it.add = None   # make it read-only

        # This iterator class is just a wrapper for a normal list.
        # Force the reduce job to use a read-only iterator API.

        class Iterator:
            def __init__(self):
                self._lst = [ ]
            def add(self, x):
                self._lst.append(x)
            def __iter__(self):
                return self
            def next(self):
                try:
                    r, self._lst = self._lst[0], self._lst[1:]
                    return r
                except IndexError:
                    raise StopIteration

        # Do the reduce jobs, and run the output writer.

        outputIterators = { }
        keys = v2iters.keys()
        if DEBUGLEVEL > 0:
            print len(keys), "reduce jobs"
        for k2 in keys:
            def emit(k2, v2):
                if not outputIterators.has_key(k2):
                    outputIterators[k2] = Iterator()
                outputIterators[k2].add(v2)
            self.Reduce(k2, v2iters[k2], emit)
            outputIterators[k2].add = None   # make it read-only

        self.OutputWriter(outputIterators)
