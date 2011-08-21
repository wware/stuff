#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

#include "jsi.h"
#include "hashtable.h"

#define JSI_QUEUE_LENGTH  20

struct jsiEvent {
    int eventtype;
    void *arg;
#if JSIDEBUG
    const char *eventname;
#endif
};

struct jsiHandlerLookup {
    struct jsiLabel *next;
    int eventtype;
    void *value;
};

#if JSIDEBUG
struct jsiLabel {
    struct jsiLabel *next;
    const char *name;
    void *value;
};
#endif

struct jsiThread {
    struct jsiEvent events[JSI_QUEUE_LENGTH];
    unsigned write_pointer;
    unsigned read_pointer;
    unsigned available;  // 0 = empty, JSI_QUEUE_LENGTH = full
    pthread_t pthread;
    pthread_mutex_t mutex; // protects access to event queue and hashtables
#if JSIDEBUG
    struct jsi_hashtable eventNames;
    struct jsi_hashtable handlerNames;
#endif
    struct jsi_hashtable handlers;
};

static struct jsiThread main_thread, task_thread;

static void *main_thread_run(void *);
static void *task_thread_run(void *);

static void jsi_init(void)
{
    main_thread.write_pointer = 0;
    main_thread.read_pointer = 0;
    main_thread.available = 0;
    task_thread.write_pointer = 0;
    task_thread.read_pointer = 0;
    task_thread.available = 0;
    pthread_mutex_init(&main_thread.mutex, NULL);
    pthread_mutex_init(&task_thread.mutex, NULL);
    jsi_hashtable_init(&main_thread.handlers);
    jsi_hashtable_init(&main_thread.handlers);
#if JSIDEBUG
    jsi_hashtable_init(&main_thread.eventNames);
    jsi_hashtable_init(&main_thread.handlerNames);
    jsi_hashtable_init(&task_thread.eventNames);
    jsi_hashtable_init(&task_thread.handlerNames);
#endif
    pthread_create(&main_thread.pthread, NULL,
                   main_thread_run, &main_thread);
    pthread_create(&task_thread.pthread, NULL,
                   task_thread_run, &task_thread);
}

#if JSIDEBUG
int _jsi_register_handler(int eventtype,
                          const char *eventname,
                          void (*handler)(void *arg))
#else
int jsi_register_handler(int eventtype,
                          void (*handler)(void *arg))
#endif
{
    int r = pthread_mutex_trylock(&main_thread.mutex);
    if (r == 0) {
        jsi_hashtable_add(&main_thread.handlers,
                          eventtype, (int) handler);
        pthread_mutex_unlock(&main_thread.mutex);
        return 0;
    } else {
        return 1;
    }
}

#if JSIDEBUG
int _jsi_register_task(int eventtype,
                       const char *eventname,
                       void (*task)(void *arg))
#else
int jsi_register_task(int eventtype,
                      void (*task)(void *arg))
#endif
{
    int r = pthread_mutex_trylock(&task_thread.mutex);
    if (r == 0) {
        jsi_hashtable_add(&task_thread.handlers,
                          eventtype, (int) task);
        pthread_mutex_unlock(&task_thread.mutex);
        return 0;
    } else {
        return 1;
    }
}

#if JSIDEBUG
int _jsi_enqueue(int eventtype, const char *eventname, void *arg)
#else
int jsi_enqueue(int eventtype, void *arg)
#endif
{
    int r = pthread_mutex_trylock(&main_thread.mutex);
    if (r == 0) {
        if (main_thread.available != JSI_QUEUE_LENGTH) {
            struct jsiEvent *evt =
                &main_thread.events[main_thread.write_pointer];
            evt->eventtype = eventtype;
            evt->arg = arg;
            main_thread.write_pointer =
                (main_thread.write_pointer + 1) % JSI_QUEUE_LENGTH;
            main_thread.available++;
        }
        pthread_mutex_unlock(&main_thread.mutex);
        return 0;
    } else {
        return 1;
    }
}

static int jsi_queue_get(struct jsiThread *thread,
                         int *eventtype, void **arg)
{
    int r = pthread_mutex_trylock(&thread->mutex);
    if (r == 0) {
        if (thread->available != 0) {
            struct jsiEvent *evt =
                &thread->events[thread->read_pointer];
            *eventtype = evt->eventtype;
            *arg = evt->arg;
            thread->read_pointer =
                (thread->read_pointer + 1) % JSI_QUEUE_LENGTH;
            thread->available--;
        }
        pthread_mutex_unlock(&thread->mutex);
        return 0;
    } else {
        return 1;
    }
}

#if JSIDEBUG
int _jsi_task_enqueue(int eventtype, const char *eventname, void *arg)
#else
int jsi_task_enqueue(int eventtype, void *arg)
#endif
{
    int r = pthread_mutex_trylock(&task_thread.mutex);
    if (r == 0) {
        if (task_thread.available != JSI_QUEUE_LENGTH) {
            unsigned int write_pointer =
                (task_thread.read_pointer +
                 JSI_QUEUE_LENGTH - task_thread.available)
                % JSI_QUEUE_LENGTH;
            struct jsiEvent *evt =
                &task_thread.events[write_pointer];
            evt->eventtype = eventtype;
            evt->arg = arg;
            task_thread.available++;
        }
        pthread_mutex_unlock(&task_thread.mutex);
        return 0;
    } else {
        return 1;
    }
}

unsigned int jsi_queue_available(void)
{
    return main_thread.available;
}

unsigned int jsi_task_queue_available(void)
{
    return task_thread.available;
}

static void *main_thread_run(void *arg)
{
    while (1) {
        if (main_thread.available != 0) {
            int eventtype = -1; void *arg;
            void (*handler)(void *arg);
            jsi_queue_get(&main_thread, &eventtype, &arg);
            if (eventtype != -1) {
                handler = (void(*)(void*))
                    jsi_hashtable_lookup(&main_thread.handlers,
                                         eventtype);
                if (handler != NULL)
                    (*handler)(arg);
            }
        }
        sched_yield();
    }
    return arg;
}

static void *task_thread_run(void *arg)
{
    while (1) {
        if (task_thread.available != 0) {
            int eventtype = -1; void *arg;
            void (*handler)(void *arg);
            jsi_queue_get(&task_thread, &eventtype, &arg);
            if (eventtype != -1) {
                handler = (void(*)(void*))
                    jsi_hashtable_lookup(&task_thread.handlers,
                                         eventtype);
                if (handler != NULL)
                    (*handler)(arg);
            }
        }
        sched_yield();
    }
    return arg;
}

// =============================================
// This little piece represents the application code.

void kbd_input(void *arg)
{
    char c = (char) ((int) arg);
    if (arg != NULL)
        printf("%c", c);
    if (c == '\n')
        jsi_enqueue(100, NULL);
}

void quit_handler(void *arg)
{
    printf("Quitting.\n");
    exit(0);
}

// ===============================================

int main(void)
{
    char *p;
    jsi_init();
    jsi_register_handler(KEYBOARD_INPUT, kbd_input);
    jsi_register_handler(100, quit_handler);
    for (p = "Here is a string.\n"; *p != '\0'; p++) {
        while (jsi_enqueue(KEYBOARD_INPUT, (void*) ((int) *p)) != 0);
    }
    pthread_join(main_thread.pthread, NULL);
    pthread_join(task_thread.pthread, NULL);
    return 0;
}
