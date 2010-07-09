import os, string, mapreduce

class MyApp(mapreduce.MapReduce):
    def __init__(self):
        self.xyzt = (3, 5, 7, 11)
    def rand(self):
        # Generating good pseudo-random numbers
        # Wichmann and Hill, Dec 2005
        # http://www.eurometros.org/file_download.php?file_key=247
        x, y, z, t = self.xyzt
        x, y, z, t = ((11600 * x) % 2147483579,
                      (47003 * y) % 2147483543,
                      (23000 * z) % 2147483423,
                      (33000 * t) % 2147483123)
        W = ((x / 2147483579.0) + (y / 2147483543.0) +
             (z / 2147483423.0) + (t / 2147483123.0))
        self.xyzt = x, y, z, t
        return W - int(W)

    def InputReader(self, emit):
        for i in range(1000):
            emit(i, self.rand())

    def Map(self, k1, v1, emit):
        emit(0, int(10000 * v1))
        emit(1, v1)

    def Reduce(self, k2, v2iter, emit):
        # test to make sure that the v2 values are in the right order
        result = 0
        if k2 == 0:
            for y in v2iter:
                result = ((117 * result) + y) % 1000000
        else:
            for y in v2iter:
                result = ((0.117 * result) + y) % 1.0
        emit(k2, result)

    def OutputWriter(self, outputQueues):
        def NoNext(iter):
            try:
                iter.next()
                assert False
            except StopIteration:
                pass
        assert len(outputQueues.keys()) == 2
        # print outputQueues[0].next()
        assert outputQueues[0].next() == 67879
        NoNext(outputQueues[0])
        # print outputQueues[1].next()
        assert 0.871521 < outputQueues[1].next() < 0.871522
        NoNext(outputQueues[1])

MyApp().Run()
MyApp().RunWithRandomizedMaps()
print 'ok'
