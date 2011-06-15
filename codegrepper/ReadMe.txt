README for code grepper

Wherever I work as a software engineer, I'll be grepping source code a
lot. I'll want to do it fast and I'll want my code grepper to be
cross-platform. So my thought is to do it in Python like this.

Make it a Python library, and later you'll write wrappers that are
customized for the environment. Now you're writing the library. Don't
worry about stuff that can easily be done in wrapper scripts. Just
make sure it's possible and efficient.

A query is three characters, case-insensitive, letters and digits.
Anything bigger than that, or case sensitivity, gets handled in
post-processing. The grepper's job is to translate a 3-char query into
a list of files as fast as possible.

With an alphabet of N characters, there are N^3 possible queries. So
we have that many files, each of them is a list of bits. That list of
bits is used to filter the list of source files. So each source file
adds some amount to this part of the index.

Indexing works like this. For each source file, you generate a
(N^3)-bit file. Then you merge those files to get the index files.

You need to be able to handle a change in one source file efficiently.
Then it's easy to write a background process that updates the index
when source files change.

====

OK, that was how I planned it originally, but it's possible that the
N^3 tree will be so sparse that we can hope to pull the whole thing
into RAM, for smaller codebases. So at the moment I'm using Python's
data structures, and just cPickling them for storage.

This stuff is now in my Mercurial repo and undergoing continuing
improvement. The code in the managed branch of the project is enough
of a challenge for right now. Maybe later I'll think harder about data
structures.

My current complaint with it is that for it to work well, you need to
use it in a captive user interface. That's because it's too expensive
to reload the data from a file for each search.

Maybe it operates as a client that makes a request of a server, and
the server keeps the data warm. Then the same server process can
handle updating the data when a source file changes. So every minute
or so, it scans the source files to see if any have gotten updated. Or
you can force an update when you want one.

So the library needs a TCP server and client for all this stuff.
