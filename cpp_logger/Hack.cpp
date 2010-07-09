/**
 * g++ -o hack Hack.cpp -lrt && ./hack
 */

#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <malloc.h>
#include <semaphore.h>

#include <iostream>
using namespace std;

static sem_t clog_list_sync;
static int clog_list_ready = 0;

/*
 * There's probably some clever thing in the C++ STL that would take
 * care of a lot of this.
 */
struct clog_list_entry {
	string filename;
	int linenumber;
	string message;
	int value;
};
#define CLOG_LISTSIZE 1000
struct clog_list_entry clog_list[CLOG_LISTSIZE];
static int clog_read_pointer = 0;
static int clog_write_pointer = 0;
static int kill_clog_thread = 0;

/* calls to this must be protected by claim and release */
static int clog_list_size(void)
{
	int x;
	x = clog_write_pointer - clog_read_pointer;
	while (x < 0) x += CLOG_LISTSIZE;
	while (x >= CLOG_LISTSIZE) x -= CLOG_LISTSIZE;
	return x;
}

static inline void print_right_justified(string str)
{
	static const char *thirty_spaces = "                              ";
	if (str.empty()) return;
	int n = str.length();
	if (n > 30) n = 30;
	cout << (thirty_spaces + n) << str;
}

static void *clog_print_thread(void *data)
{
	(void) data;
	while (!kill_clog_thread) {
		if (sem_wait(&clog_list_sync) != 0)
			break;
		if (clog_list_size() > 0) {
			if (!clog_list[clog_read_pointer].filename.empty()) {
				print_right_justified(clog_list[clog_read_pointer].filename);
				printf(":%d\t", clog_list[clog_read_pointer].linenumber);
				print_right_justified(clog_list[clog_read_pointer].message);
				printf(" %12d %010p\n", clog_list[clog_read_pointer].value, clog_list[clog_read_pointer].value);
			} else {
				printf("  * * * * C logger dropped %d messages * * * *\n",
				       clog_list[clog_read_pointer].linenumber);
			}
			clog_read_pointer = (clog_read_pointer + 1) % CLOG_LISTSIZE;
		}
		sem_post(&clog_list_sync);
	}
	return data;
}

void put_clog_message(string filename, int linenumber, string message, int value)
{
	if (!clog_list_ready) {
	    pthread_t thr;
	    sem_init(&clog_list_sync, 0, 1);
	    pthread_create(&thr, NULL, clog_print_thread, NULL);
	    clog_list_ready = 1;
	}
	if (sem_wait(&clog_list_sync) != 0)
		return;
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
		if (!clog_list[previous_write].filename.empty()) {
			clog_list[clog_write_pointer].filename = "";
			clog_list[clog_write_pointer].linenumber = 1;
			clog_list[clog_write_pointer].message = "";
			clog_list[clog_write_pointer].value = 0;
			clog_write_pointer = (clog_write_pointer + 1) % CLOG_LISTSIZE;
		} else {
			// single message already stored, increment number dropped
			clog_list[previous_write].linenumber++;
		}
	}
	sem_post(&clog_list_sync);
}

extern void put_clog_message(string filename, int linenumber, string message, int value);
#define DBGMSG(msg,val)  put_clog_message(__FILE__, __LINE__, msg, (val))
#define VALUE(val)       DBGMSG(#val,val)
#define OBJ(obj)         put_clog_message(__FILE__, __LINE__, obj->name(), ((int) obj))

class Object
{
public:
	string toString(void)
	{
		char buf[20];
		string s = this->name();
		s.append("@0x");
		sprintf(buf, "@0x%X", (void*) this);
		s.append(buf);
		return s;
	}
	virtual string name(void) = 0;
};

class ExampleObject : public Object
{
public:
	string name(void)
	{
		return "ExampleObject";
	}
};

int main(void)
{
	ExampleObject *obj = new ExampleObject();
	int i, pi = 314159;
	for (i = 0; i < 100000; i++) {
		VALUE(pi);
		VALUE(pi + pi);
		VALUE(pi * pi + pi);
		OBJ(obj);
	}
	sleep(1);
}
