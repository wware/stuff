#ifndef JSI_H_INCLUDED
#define JSI_H_INCLUDED

/**
 * If this is 1, queues will record names for events, handlers, and tasks,
 * which makes it possible to write profiling tools for the JSI layer.
 */
#define JSIDEBUG 1

#if JSIDEBUG

#define jsi_register_handler(eventtype,handler) \
    _jsi_register_handler(eventtype,#handler,handler)
extern int _jsi_register_handler(int eventtype,
                                 const char *handlername,
                                 void (*handler)(void *arg));

#define jsi_register_task(eventtype,task) \
    _jsi_register_task(eventtype,#task,task)
extern int _jsi_register_task(int eventtype,
                              const char *taskname,
                              void (*task)(void *arg));

#define jsi_enqueue(eventtype,arg) \
    _jsi_enqueue(eventtype,#eventtype,arg)
extern int _jsi_enqueue(int eventtype,
                        const char *eventname,
                        void *arg);

#define jsi_task_enqueue(eventtype,arg) \
    _jsi_task_enqueue(eventtype,#eventtype,arg)
extern int _jsi_task_enqueue(int eventtype,
                             const char *eventname,
                             void *arg);

#else // JSIDEBUG

/**
 * Register a handler for the main thread.
 */
extern int jsi_register_handler(int eventtype, void (*handler)(void *arg));

/**
 * Register a handler for the task threads.
 */
extern int jsi_register_task(int eventtype, void (*task)(void *arg));

/**
 * Enqueue an event of a given type with a particular argument.
 */
extern int jsi_enqueue(int eventtype, void *arg);

/**
 * Enqueue an event of a given type with a particular argument in the task
 * event queue.
 */
extern int void jsi_task_enqueue(int eventtype, void *arg);

#endif // JSIDEBUG

/**
 * Tells how many spots are available in the main event queue. If this
 * function returns zero, jsi_enqueue will do nothing.
 */
extern unsigned int jsi_queue_available(void);

/**
 * Tells how many spots are available in the task event queue.
 */
extern unsigned int jsi_task_queue_available(void);

enum {
    KEYBOARD_INPUT = 0,
    MOUSE_DOWN,
    MOUSE_MOVE,
    MOUSE_UP,
    NETWORK_PACKET_RECEIVED
};

#endif // JSI_H_INCLUDED
