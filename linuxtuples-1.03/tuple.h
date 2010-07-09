/*****************************************************************
 *
 * LinuxTuples - an open-source tuple system for Linux clusters
 * Copyright (c) 2003, Will Ware <wware@alum.mit.edu>
 * All rights reserved.
 * 
 *    Redistribution and use in source and binary forms, with or
 *    without modification, are permitted provided that the following
 *    conditions are met:
 * 
 *    + Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 *    + Redistributions in binary form must reproduce the above
 *    copyright notice, this list of conditions and the following
 *    disclaimer in the documentation and/or other materials provided
 *    with the distribution.
 *
 *    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
 *    CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 *    INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 *    MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 *    DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 *    CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 *    LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 *    USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 *    AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 *    IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 *    THE POSSIBILITY OF SUCH DAMAGE.
 *
 *****************************************************************/

#ifndef TUPLE_H_INCLUDED
#define TUPLE_H_INCLUDED

#ifndef va_list
#define va_list __gnuc_va_list
#endif

#ifdef DEBUG
#define _DBG_(x)  x
#else
#define _DBG_(x)
#endif

#define ASSERT(cond)   \
   _DBG_(if (!(cond)) { DBGPRINTF("assert failed: %s\n", \
                                  #cond); exit(1); })

#define DBGPRINTF(fmt,a...)   \
   _DBG_(fprintf(stderr, "%d  " __FILE__ ":%d " fmt, \
                 debug_counter++, __LINE__, ##a))


#define PRINTF(fmt,a...)   \
   fprintf(stderr, __FILE__ ":%d  " fmt, __LINE__, ##a)


#define PERROR(x) \
  fprintf(stderr, __FILE__":%d  ", __LINE__); perror(x)


#define TELL(x) \
   DBGPRINTF(#x " = %d\n", x)

#if defined(DEBUG)
static inline void *
mymalloc(int size, char *file, int line)
{
	void *p;
	p = malloc(size);
	fprintf(stderr, "%s:%d malloc %08x\n", file, line, (unsigned int) p);
	return p;
}
static inline void
myfree(void *p, char *file, int line)
{
	fprintf(stderr, "%s:%d free %08x\n", file, line, (unsigned int) p);
	free(p);
}

#define MALLOC(n) mymalloc(n,__FILE__,__LINE__)
#define FREE(n) myfree(n,__FILE__,__LINE__)
#else
#define MALLOC malloc
#define FREE free
#endif

#ifdef DEBUG
/* Get a stack trace for GDB*/
#define EXIT()   *((int*) 0) = 11
#else
#define EXIT()   exit(1)
#endif

/* Time difference between two struct timevals.
 */
#define TIMEVAL_DIFF(before,after)  \
	((after.tv_sec - before.tv_sec) + \
	1.0e-6 * (after.tv_usec - before.tv_usec))


#if 1
#define SHUTDOWN_SOCKET(S, HOW) \
	if (shutdown(S, HOW)) \
	{ \
		PERROR("shutdown"); \
	}
#else
#define SHUTDOWN_SOCKET(S, HOW) ;
#endif


/* Environment variables */
#define SERVERNAME_ENVVAR  "LINUXTUPLES_HOST"
#define PORTNUMBER_ENVVAR  "LINUXTUPLES_PORT"

enum message_op
{
	PUT = 0,
	GET = 1,
	READ = 2,
	GET_NB = 3,
	READ_NB = 4,
	DUMP = 5,
	COUNT = 6,
	LOG = 7,
	REPLACE = 8
};

/**
 * \brief The representation of a tuple element in C code, in both the
 * server and a C client.
 */
struct element
{
	/**
	 * Data type for this element. The values are `i' for integer,
	 * `d' for double, `?' for wildcard, and `s' for string.
	 * Because the Intel architecture is little-endian, the ASCII
	 * character occupies the first byte in memory (and the first
	 * one to be shipped over the network) and the other three
	 * bytes are zeroes. That sounds inefficient, but if I declare
	 * ``char tag'', the storage size for ``struct element'' is
	 * still 12 bytes. The knowledge that those three bytes are
	 * zeroes could, in future, be used to compress the network
	 * protocol.
	 *
	 * Since I'm only concerned with Linux running on Intel
	 * hardware, it's safe to say this is a four-byte
	 * little-endian field.
	 */
	int tag;
	/**
	 * Since I'm only concerned with Linux running on Intel
	 * hardware, it's safe to say this union is an eight-byte
	 * field.
	 */
	union
	{
		/**
		 * Integer value for a tuple element. Four bytes on an
		 * Intel Linux box.
		 */
		int i;
		/**
		 * Double-precision floating-point value for a tuple
		 * element. Eight bytes on an Intel Linux box.
		 */
		double d;
		/**
		 * String value for a tuple element. Strings can be
		 * ASCII strings, or they can be a handy efficient way
		 * to move around binary data, such as arrays of
		 * doubles or structs.
		 */
		struct
		{
			/**
			 * A pointer to the memory space for a string.
			 */
			char *ptr;
			/**
			 * The length of a string. We do not depend on
			 * zero-termination bytes for strings, because
			 * we want to be able to use strings to move
			 * arbitrary binary data around. Strings can
			 * be large.
			 */
			int len;
		}
		s;
		struct tuple *t;
	}
	data;
};

/**
 * \brief An ordered sequence of data values: integers, floats,
 * strings. If this is a template (used during GET and READ operations
 * to match existing tuples), an element can also be a wildcard.
 */
struct tuple
{
	/**
	 * The number of elements in this tuple.
	 */
	int num_elts;
	/**
	 * The total length of all the strings in this tuple. Useful
	 * for planning storage allocation.
	 */
	int string_length;
	/**
	 * The array of elements for this tuple.
	 */
	struct element *elements;
	/**
	 * A pointer to storage space for the tuple's strings. This
	 * pointer may not need to be used, if the strings already
	 * have their own memory when the tuple is created. The time
	 * when you need to use this pointer is when you're receiving
	 * a tuple over the network. If the tuple was constructed
	 * using make_tuple() or put_tuple(), then its strings are
	 * assumed to already have memory space.
	 */
	char *string_space;
};

/**
 * \brief Convenience struct, for when somebody needs to make a quick
 * list of tuples.
 */
struct tuple_list
{
	struct tuple_list *next;
	struct tuple *tup;
};


/**
 * \brief The information we need to keep track of a client-server
 * connection.
 *
 * From the client's point of view, this includes the hostname and
 * port number. From the server's point of view this includes a
 * pthread for the client. Both client and server maintain a socket
 * descriptor here.
 */
struct context
{
	/**
	 * A unique identifier for the context
	 * This may help in bug hunts.
	 */
	int id;
	/**
	 * The hostname of the peer
	 */
	char peername[100];
	/**
	 * The port number of the tuple server. Used by the client to
	 * make connections. Ignored in server code.
	 */
	int portnumber;
	/**
	 * Socket for connection between client and tuple server. 
	 */
	int sock;
	/**
	 * On the server side, this is the pthread for the client. If
	 * no pthread is active here, this is set to -1 so we'll know
	 * it's available.
	 */
	pthread_t thr;
};



/* ================= tuple.c ================= */
#define LOGBUFSIZE 32768

#ifdef DEBUG
extern int debug_counter;
#endif
extern int i_am_server;
extern char logbuf[LOGBUFSIZE];
extern int logptr;
extern int get_server_portnumber(struct context *ctx);
extern void print_element(struct element *e);
extern void print_tuple(struct tuple *s);
extern struct tuple *make_tuple(char *fmt, ...);
extern void destroy_tuple(struct tuple *t);
extern int tuples_match(struct tuple *s, struct tuple *t);
extern unsigned int random_int(void);
extern int send_chunk(struct context *ctx, char *buf, int bytes_to_send);
extern int send_tuple(struct context *ctx, struct tuple *t);
extern int recv_chunk(struct context *ctx, char *buf, int size);
extern struct tuple *recv_tuple(struct context *ctx);
extern int put_tuple(struct tuple *s, struct context *ctx);
extern int replace_tuple(struct tuple *template, struct tuple *replacement, struct context *ctx);
extern struct tuple *get_tuple(struct tuple *s, struct context *ctx);
extern struct tuple *read_tuple(struct tuple *s, struct context *ctx);
extern struct tuple *get_nb_tuple(struct tuple *s, struct context *ctx);
extern struct tuple *read_nb_tuple(struct tuple *s, struct context *ctx);
extern struct tuple_list *dump_tuple_space(struct tuple_list *templates,
					   struct context *ctx);
extern int count_tuple_space(struct tuple_list *templates,
			     struct context *ctx, int *result);
extern int tuple_server_log(FILE * stream, struct context *ctx);
extern int tuple_int_field(struct tuple *s, int n);
extern double tuple_double_field(struct tuple *s, int n);
extern char *tuple_string_field(struct tuple *s, int *len, int n);

#endif /* TUPLE_H_INCLUDED */
