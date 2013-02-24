Queueing jobs with Redis and Python
===================================

Some years back, I read David Gelernter's book "Mirror Worlds" and got interested in his notion of
a tuple space, which is a way to organize distributed computation. I wrote a little tuple space
server for Linux and posted it on Sourceforge, and it got a few users. A couple of the users found
it very useful for large jobs, and having a much larger environment to work in than I had, found
and fixed a few performance bugs in my code. That was a rewarding experience and I still mention
it on my resume.

Writing my own little database in C from scratch is obviously not a great idea, when there is now
such a profusion of databases with large user communities and large bodies of experience and
development effort behind them. So I no longer bother to maintain LinuxTuples.

I think the next thing to replace it should be a Python wrapper for Redis. There is already a good
Redis client for Python (see https://github.com/andymccurdy/redis-py and
http://degizmo.com/2010/03/22/getting-started-redis-and-python/) and my intention is to create a
base class for a distributed job queue, with some examples and tutorials.

Redis Cluster isn't yet ready, but there is something a little like it called Redis Sharding, see
https://github.com/kni/redis-sharding. It's pretty simple, you compute a CRC32 of the key, and use
that to select from a list of hosts. The implementation is in Perl, but certainly the same thing
can be easily done in Python.

In the past I didn't have access to a large distributed environment in which to work, having only
a handful of Linux machines in my house. But now we have things like Google Compute Engine (see
https://cloud.google.com/products/compute-engine) and Amazon Elastic Cloud (see http://aws.amazon.com/ec2/)
where, for a small amount of money, I can throw a large number of machines at some problem, provided
I can find an interesting problem to attack.

The problem might be something in number theory, or something in molecular modeling (I once designed
a space partitioning scheme for this purpose, see http://www.foresight.org/Conferences/MNT05/Papers/Ware/)
or some other approach (see http://www.biostars.org/p/3977/, candidates include GROMACS, AMBER, NAMD,
and others).

I think the base class should include the following pieces:

* instances can be serialized and deserialized (including child objects) using cPickle: strings
  in Redis may be as large as 512 Mbytes
* methods for enqueueing and dequeueing jobs awaiting computation, in a named queue
* methods to place results in either a result queue (when order does not matter) or a hash (when it
  does, for example, molecular trajectory snapshots, or movie frames)

If I choose a few wildly different jobs and have a few different distributed computing environments,
I can do a good job of partitioning this organizational stuff from the actual computation stuff.

There are jobs where the data is really big and the processes are small, and in those cases you
want to set up the job to run on a particular machine because the data is already there. So there
needs to be a way to modify the job-dequeueing methods to make that happen.
