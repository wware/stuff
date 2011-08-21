JSI is a coding style and thin infrastructure layer for small-memory MMU-less
embedded platforms. JSI maintains a main thread which works like the event
handler in JavaScript, Android or iOS. Events can arise from hardware events
(such as a button press or a UART interrupt or receiving an Ethernet packet)
or can be posted from user-written code. Events enter a queue and are handled
sequentially, each handler running to completion before the next handler runs,
as in JavaScript. JSI can run on a laptop on Linux (during application
development) or embedded OSes such as FreeRTOS for deployment.

In addition to the main event-handling thread, in which handlers should run as
quickly as possible to maintain responsiveness, there are one or more threads
to handle background tasks outside the main thread. Tasks are written as
handlers fed from a second event queue called the task queue. In an
implementation with multiple task threads, the task threads will automatically
load balance simply by popping tasks off the queue as threads become available
for work.

JSI users should rely entirely on queues for communication between event
handlers and tasks. This is done by completely avoiding global variables or
data structures. Programmming this way eliminates the need for concurrency
methods (locks, mutexes, semaphores) and makes your code much simpler.

 TODO: Provide profiling tools to maintain logs of each thread's activity.
 This requires that queues record human-readable names for events, tasks,
 and handlers. This can be done with C macros.
