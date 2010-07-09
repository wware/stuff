# Do a little unit testing to make sure the new count feature is
# working correctly.

import linuxtuples, unittest

conn = linuxtuples.connect()

class LinuxTuplesTestCase(unittest.TestCase):

    def setUp(self):
        # Empty the tuple space
        for i in range(1, 8):
            template = tuple(i * [None,])
            while conn.get_nonblocking(template) != None:
                pass
        # This exposed an interesting bug: a zero-length tuple will
        # cause get_nonblocking to just sit there forever, which it
        # should never do.

    def testLinuxTuples(self):
        self.assertEquals(conn.count(), 0)

        conn.put((1,2))
        conn.put((1,2,3))
        conn.put((2,4,6))
        self.assertEquals(conn.count(), 3)
        self.assertEquals(conn.count([(1,None)]), 1)
        self.assertEquals(conn.count([(1,None,None)]), 1)
        self.assertEquals(conn.count([(1,None,None),(None,4,None)]), 2)
        self.assertEquals(conn.count([(2,None,None),(None,4,None)]), 1)

        conn.get((None,2,None))
        self.assertEquals(conn.count(), 2)
        self.assertEquals(conn.count([(1,None)]), 1)
        self.assertEquals(conn.count([(1,None,None)]), 0)
        self.assertEquals(conn.count([(2,None,None)]), 1)
        self.assertEquals(conn.count([(1,None,None),(None,4,None)]), 1)
        self.assertEquals(conn.count([(2,None,None),(None,4,None)]), 1)

        conn.get((None,None,6))
        self.assertEquals(conn.count(), 1)
        self.assertEquals(conn.count([(1,None)]), 1)
        self.assertEquals(conn.count([(1,None,None)]), 0)
        self.assertEquals(conn.count([(2,None,None)]), 0)
        self.assertEquals(conn.count([(1,None,None),(None,4,None)]), 0)
        self.assertEquals(conn.count([(2,None,None),(None,4,None)]), 0)

        conn.get((None,None))
        self.assertEquals(conn.count(), 0)
        self.assertEquals(conn.count([(1,None)]), 0)
        self.assertEquals(conn.count([(1,None,None)]), 0)
        self.assertEquals(conn.count([(2,None,None)]), 0)
        self.assertEquals(conn.count([(1,None,None),(None,4,None)]), 0)
        self.assertEquals(conn.count([(2,None,None),(None,4,None)]), 0)

if __name__ == '__main__':
    unittest.main()
