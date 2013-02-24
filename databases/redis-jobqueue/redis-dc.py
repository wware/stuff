# Redis-based distributed computing

import cPickle
import types
import redis
import binascii
import sys

# Assign server when a list of Redis servers is available.
server = None

class ShardedRedis:
    """Sharding is done by taking a CRC32 of the key and using it to index
    into the list of available Redis servers."""

    def __init__(self, serverlist, op=None):
        self.serverlist = serverlist
        self.n = len(serverlist)
        if op is None:
            for op in filter(lambda x: not x.startswith("_"),
                             dir(redis.Redis)):
                setattr(ShardedRedis, op, ShardedRedis(serverlist, op))
        else:
            self.op = op

    def __call__(self, *kv):
        server = self.serverlist[binascii.crc32(kv[0]) % self.n]
        return apply(getattr(server, self.op), kv)

class Job:

    def enqueue(self, qname):
        server.lpush(qname, cPickle.dumps(self))

    def do(self):
        pass   # overload me


def dequeue(qname):
    kv = server.brpop(qname, 0)
    assert kv is not None, qname
    return cPickle.loads(kv[1])

def workerBee(qname, finished=lambda: false):
    while True:
        dequeue(qname).do()
        # where do we put the result? overload compute to handle that?
        if finished():
            return

###############################

# list can include any number of redis servers
server = ShardedRedis([
        redis.Redis("localhost"),
        ])

class Example(Job):

    def __init__(self, i):
        self.i = i

    def do(self):
        self.i *= 7
        self.enqueue("answers")

###############################

server.delete("questions")
server.delete("answers")

N = 20

for i in range(N):
    e = Example(i)
    e.enqueue("questions")

# keep going until no questions are left
workerBee("questions", lambda: server.llen("questions") == 0)

while server.llen("answers") != 0:
    print dequeue("answers").i
