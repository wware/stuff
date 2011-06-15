Fooling around with memory allocation
=====================================

Memory allocation is one of those classically hard problems of computer
science, or so I had been led to believe. So imagine my surprise when I came
across a really simple elegant memory allocator as part of the FreeRTOS
operating system.

http://code.google.com/p/lumweb/source/browse/trunk/src/external/freeRTOS/Source/portable/MemMang/heap_2.c

The free list is maintained as a linked list of blocks in order of increasing
size. The malloc() function steps through them, looking for the first one
large enough to accommodate a request. If the block is larger than the request
by a sufficient margin, it is split into two blocks, the first one returned,
and the second kept in the free list. When a block is freed, it is returned to
the free list, retaining the ordering by block size. This is done by the
prvInsertBlockIntoFreeList function. Each block begins with a struct
containing the next-pointer for the size-ordered linked list, and a byte
count. The allocation (the pointer returned by malloc) comes right after
the struct. There are likely to be word alignment issues to think about, for
example I am planning to use this on a 32-bit architecture so the smallest
possible allocation would be four bytes.

It's quite ingenious but it's prone to fragmentation. If you have lots of very
small allocations they can chop the heap into small pieces, so that a later
large allocation request is unnecessarily denied even though there is really
enough free memory to serve it.

I've got an idea for defragmentation which I will be tinkering with here. The
idea is that when you free a block, you look at the immediately preceding and
following blocks, not in order of increasing block size but in order of
physical memory address, and you look to see, what is the largest contiguous
block I can reassemble right now? If two or more blocks are contiguous (they
have zero bytes between them) then they can be merged into one big block.

A couple of minor details. The struct at the beginning of each block will need
one more next-pointer for the ordering by physical address. The time spent in
free() will be a little more, and during that time, any thread that might want
to call malloc() or free() will need to be suspended (as is done in FreeRTOS
already).

I plan to try implementing this and writing some tests to see if it works as
well as I hope. In particular it should be possible to do the worst-case thing
above of many small allocations followed by one very large allocation. If it
passes tests, I'll suggest it to the FreeRTOS folks as an alternative to
heap_2.c.
