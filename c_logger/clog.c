/*
 * C logger, tries to minimize performance hit of logging a message.
 * Potentially useful in embedded systems.
 *
 * gcc -o clog clog.c -lrt
 */

#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <malloc.h>
#include <semaphore.h>

static sem_t clog_list_sync;
static int clog_list_ready = 0;

struct clog_list_entry {
	char *filename;
	int linenumber;
	char *message;
	int value;
};
#define CLOG_LISTSIZE 1000
struct clog_list_entry clog_list[CLOG_LISTSIZE];
static int clog_read_pointer = 0;
static int clog_write_pointer = 0;
static int kill_clog_thread = 0;

static inline int claim_lock(void)
{
	return sem_wait(&clog_list_sync);
}

static inline void release_lock(void)
{
	sem_post(&clog_list_sync);
}

/* calls to this must be protected by claim and release */
static int clog_list_size(void)
{
	int x;
	x = clog_write_pointer - clog_read_pointer;
	while (x < 0) x += CLOG_LISTSIZE;
	while (x >= CLOG_LISTSIZE) x -= CLOG_LISTSIZE;
	return x;
}

static inline void print_right_justified(char *str)
{
	static const char *thirty_spaces = "                              ";
	if (str == NULL) return;
	int n = strlen(str);
	if (n > 30) n = 30;
	printf("%s", thirty_spaces + n);
	printf("%s", str);
}

static void *clog_print_thread(void *data)
{
	(void) data;
	while (!kill_clog_thread) {
		if (claim_lock() != 0) break;
		if (clog_list_size() > 0) {
			if (clog_list[clog_read_pointer].filename != NULL) {
				if (1) {
					print_right_justified(clog_list[clog_read_pointer].filename);
					printf(":%d\t", clog_list[clog_read_pointer].linenumber);
				}
				print_right_justified(clog_list[clog_read_pointer].message);
				printf(" %12d %010p\n", clog_list[clog_read_pointer].value, clog_list[clog_read_pointer].value);
			} else {
				printf("  * * * * C logger dropped %d messages * * * *\n",
				       clog_list[clog_read_pointer].linenumber);
			}
			clog_read_pointer = (clog_read_pointer + 1) % CLOG_LISTSIZE;
		}
		release_lock();
	}
	return data;
}

void put_clog_message(char *filename, int linenumber, char *message,
int value)
{
	if (!clog_list_ready) {
	    pthread_t thr;
	    sem_init(&clog_list_sync, 0, 1);
	    pthread_create(&thr, NULL, clog_print_thread, NULL);
	    clog_list_ready = 1;
	}
	if (claim_lock() != 0) return;
	if (clog_list_size() < CLOG_LISTSIZE - 2) {
		// store a message if there is room for it
		clog_list[clog_write_pointer].filename = filename;
		clog_list[clog_write_pointer].linenumber = linenumber;
		clog_list[clog_write_pointer].message = message;
		clog_list[clog_write_pointer].value = value;
		clog_write_pointer = (clog_write_pointer + 1) % CLOG_LISTSIZE;
	} else {
		// otherwise store a single message telling how many were dropped
		int previous_write =
			(clog_write_pointer + CLOG_LISTSIZE - 1) % CLOG_LISTSIZE;
		if (clog_list[previous_write].filename != NULL) {
			clog_list[clog_write_pointer].filename = NULL;
			clog_list[clog_write_pointer].linenumber = 1;
			clog_list[clog_write_pointer].message = NULL;
			clog_list[clog_write_pointer].value = 0;
			clog_write_pointer = (clog_write_pointer + 1) % CLOG_LISTSIZE;
		} else {
			// single message already stored, increment number dropped
			clog_list[previous_write].linenumber++;
		}
	}
	release_lock();
}

#if 1
#include "clog.h"

int main(void)
{
	int i, pi = 314159;
	for (i = 0; i < 100000; i++) {
		VALUE(pi);
		VALUE(pi + pi);
		VALUE(pi * pi + pi);
		VALUE(pi + pi + pi + pi + pi + pi + pi);
	}
	sleep(1);
}
#endif
