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

#include <assert.h>
#include <stdio.h>
#include <stdarg.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netinet/tcp.h>	// Added for TCP_NODELAY
#include <netdb.h>
#include <sched.h>

#ifdef MACOSX
#include <malloc/malloc.h>
#include <netinet/in.h>
#define MSG_NOSIGNAL  SO_NOSIGPIPE
#else
#include <malloc.h>
#endif

#include "tuple.h"

/**
 * This flag tells whether or not the current executable is the
 * tuple server. If it is, then broken pipes on sockets should
 * not crash the executable.
 *
 * Doxygen likes LaTeX-flavored math:
 * \f[
 * |I_2|=\left| \int_{0}^T \psi(t) 
 * \left\{ 
 * u(a,t)-
 * \int_{\gamma(t)}^a 
 * \frac{d\theta}{k(\theta,t)}
 * \int_{a}^\theta
 * c(\xi)u_t(\xi,t)\,d\xi
 * \right\}
 * dt
 * \right|
 * \f]
 */
int i_am_server = 0;

#ifdef DEBUG
int debug_counter = 0;
#endif

/**
 * Buffer for logging tuple activity.
 * \sa tuple_server_log
 */
char logbuf[LOGBUFSIZE];
/**
 * Pointer into logbuf.
 */
int logptr = 0;


#define PERROR(x) fprintf(stderr, __FILE__":%d  ", __LINE__); perror(x)

void print_tuple_stderr(struct tuple *s);
void print_tuple(struct tuple *s);
void print_element_stderr(struct element *e);
struct tuple *tuple_tuple_field(struct tuple *s, int n);

/**
 * Attempt to get the server name and port number from environment
 * variables.
 *
 * \return 0 if successful, 1 otherwise.
 */
int
get_server_portnumber(struct context *ctx)
{
	char *s;
	s = getenv(SERVERNAME_ENVVAR);
	if (s == NULL)
		return 1;
	if (strlen(s) == 0)
		return 1;
	strcpy(ctx->peername, s);
	s = getenv(PORTNUMBER_ENVVAR);
	if (s == NULL)
		return 1;
	ctx->portnumber = atoi(s);
	return 0;
}

/*
 * ----------------------------------------------------------- 
 */

/**
 *
 */
void
print_element(struct element *e)
{
	int i, n, too_long, tag = e->tag;
	unsigned char *s;
	const int max_str_len = 16;
	switch (tag) {
	case 'i':
		logptr += sprintf(logbuf + logptr, "%d", e->data.i);
		break;
	case 'd':
		logptr += sprintf(logbuf + logptr, "%f", e->data.d);
		break;
	case 's':
		s = (unsigned char*)e->data.s.ptr;
		n = e->data.s.len;
		too_long = 0;
		if (n > max_str_len) {
			n = max_str_len;
			too_long = 1;
		}
		logbuf[logptr++] = '"';
		for (i = 0; i < n; i++) {
			if (s[i] >= ' ' && s[i] <= '~') {
				logbuf[logptr++] = s[i];
			}
			else {
				logptr +=
					sprintf(logbuf + logptr,
						"\\%02X", s[i]);
			}
		}
		if (too_long) {
			logptr += sprintf(logbuf + logptr, " ...");
		}
		logbuf[logptr++] = '"';
		break;
	case 't':
		print_tuple(e->data.t);
		break;
	case '?':
		logptr += sprintf(logbuf + logptr, "???");
		break;
	default:
		logptr +=
			sprintf(logbuf + logptr,
				"<<unknown field, tag=%d>>", tag);
		break;
	}
}


/**
 *
 */
void
print_element_stderr(struct element *e)
{
	int i, n, too_long, tag = e->tag;
	unsigned char *s;
	const int max_str_len = 16;
	switch (tag) {
	case 'i':
                fprintf(stderr,"%d", e->data.i);
		break;
	case 'd':
		fprintf(stderr,"%f", e->data.d);
		break;
	case 's':
		s = (unsigned char*)e->data.s.ptr;
		n = e->data.s.len;
		too_long = 0;
		if (n > max_str_len) {
			n = max_str_len;
			too_long = 1;
		}
		fprintf(stderr,"'");
		for (i = 0; i < n; i++) {
			if (s[i] >= ' ' && s[i] <= '~') {
				fprintf(stderr,"%c",s[i]);
			}
			else {
				fprintf(stderr,"\\%02X", s[i]);
			}
		}
		if (too_long) {
			fprintf(stderr," ...");
		}
		fprintf(stderr,"'");
		break;
	case 't':
		print_tuple_stderr(e->data.t);
		break;
	case '?':
		fprintf(stderr,"???");
		break;
	}
}

/**
 *
 */
void
print_tuple(struct tuple *s)
{
	int i, n, flag = 0;
	if (logptr >= LOGBUFSIZE-128)
		return;
	logptr += sprintf(logbuf + logptr, "(");
	n = s->num_elts;
	assert(n);
	if (n > 10) {
		n = 10;
		flag = 1;
	}
	for (i = 0; i < n; i++) {
		if (i > 0) logptr += sprintf(logbuf + logptr, ", ");
		print_element(&s->elements[i]);
	}
	if (flag) {
		logptr += sprintf(logbuf + logptr, ".... ");
	}
	logptr += sprintf(logbuf + logptr, ")\n");
}


/**
 *
 */
void
print_tuple_stderr(struct tuple *s)
{
	int i, n, flag = 0;
	logptr += sprintf(logbuf + logptr, "(");
        fprintf(stderr,"(");
	n = s->num_elts;
	assert(n);
	if (n > 10) {
		n = 10;
		flag = 1;
	}
	for (i = 0; i < n; i++) {
		if (i > 0) fprintf(stderr, ", ");
		print_element_stderr(&s->elements[i]);
	}
	if (flag) {
		fprintf(stderr, ".... ");
	}
        fprintf(stderr,")");
}

/*
 * ----------------------------------------------------------- 
 */

/**
 *
 */
static struct tuple *
make_tuple_internal(char *fmt, va_list ap)
{
	int i, elt_index, len;
	double d;
	char *s;
	struct tuple *t;

	t = malloc(sizeof(struct tuple));
	if (t == NULL) {
		PERROR("malloc failed");
		EXIT();
	}
	t->num_elts = 0;
	t->string_space = NULL;
	for (s = fmt; *s; s++) {
		if (*s != '#')
			t->num_elts++;
	}
	t->elements = malloc(t->num_elts * sizeof(struct element));
	if (t->elements == NULL) {
		PERROR("malloc failed");
		EXIT();
	}

	elt_index = 0;
	while (*fmt) {
		t->elements[elt_index].tag = *fmt;
		switch (*fmt++) {
		case '?':
			/*
			 * nothing from the va_arg list 
			 */
			break;
		case 'i':
			i = va_arg(ap, int);
			t->elements[elt_index].data.i = i;
			break;
		case 'd':
			d = va_arg(ap, double);
			t->elements[elt_index].data.d = d;
			break;
		case 't':
			t->elements[elt_index].data.t = (struct tuple*) malloc(sizeof(struct tuple));
			make_tuple_internal(fmt, ap);
			break;
		case 's':
			s = va_arg(ap, char *);
			if (*fmt == '#') {
				/*
				 * string with length 
				 */
				len = va_arg(ap, int);
				fmt++;
			}
			else {
				/*
				 * zero-terminated string 
				 */
				len = strlen(s);
			}
			t->elements[elt_index].data.s.ptr = s;
			t->elements[elt_index].data.s.len = len;
			break;
		}
		elt_index++;
	}
	return t;
}

/**
 * Create a tuple or template, starting from a printf-style
 * variable argument list.
 */
struct tuple *
make_tuple(char *fmt, ...)
{
	struct tuple *t;
	va_list ap;
	va_start(ap, fmt);
	t = make_tuple_internal(fmt, ap);
	va_end(ap);
	return t;
}


/**
 * Deallocate all the storage for a tuple.
 */
void
destroy_tuple(struct tuple *t)
{
	if (t != NULL) {
		free(t->elements);
		if (t->string_space != NULL)
			free(t->string_space);
		free(t);
	}
}


/**
 *
 */
int
tuples_match(struct tuple *s, struct tuple *t)
{
	int i, num_elts;
	num_elts = s->num_elts;
	if (num_elts != t->num_elts)
		return 0;
	for (i = 0; i < num_elts; i++) {
		int s_tag = s->elements[i].tag;
		int t_tag = t->elements[i].tag;

		if (s_tag != '?' && t_tag != '?') {
			if (s_tag != t_tag)
				return 0;
			switch (s_tag) {
			case 'i':
				if (s->elements[i].data.i !=
				    t->elements[i].data.i)
					return 0;
				break;
			case 'd':
				if (s->elements[i].data.d !=
				    t->elements[i].data.d)
					return 0;
				break;
			case 's':
				if (s->elements[i].data.s.len !=
				    t->elements[i].data.s.len)
					return 0;
				if (strncmp(s->elements[i].data.s.ptr,
					    t->elements[i].data.s.ptr,
					    s->elements[i].data.s.len) != 0)
					return 0;
				break;
			case 't':
				if (! tuples_match(
					s->elements[i].data.t,
					t->elements[i].data.t))
					return 0;
			default:
				break;
			}
		}
	}
	return 1;
}



/*
 * ----------------------------------------------------------- 
 */

/**
 * Return a 32-bit random unsigned integer, gotten from
 * /dev/urandom.
 */
unsigned int
random_int(void)
{
	int x, sock;
	sock = open("/dev/urandom", O_RDONLY);
	read(sock, &x, sizeof(int));
	close(sock);
	return x;
}


/*
 * ----------------------------------------------------------- 
 */

/**
 *
 */
int
send_chunk(struct context *ctx, char *buf, int bytes_to_send)
{
	while (bytes_to_send) {
		int sent, masks = 0;
		/*
		 * The server should not get a SIGPIPE if a client
		 * dies, because then it will stop running. But we
		 * want clients to stop running (Broken pipe) if the
		 * server stops.
		 */
		if (i_am_server)
			masks = MSG_NOSIGNAL;
		sent = send(ctx->sock, buf, bytes_to_send, masks);
		if (sent < 0) {
			PERROR("send failed");
			PRINTF("ctx->id=%d, bytes_to_send=%d\n",ctx->id,bytes_to_send);
			return 1;
		}
		buf += sent;
		bytes_to_send -= sent;
		sched_yield();
	}
	return 0;
}


/**
 *
 */
int
send_tuple(struct context *ctx, struct tuple *t)
{
	int i, string_length;
	if (send_chunk(ctx, (char *) &t->num_elts, sizeof(int))) {
		PERROR("send_chunk failed");
		return 1;
	}
	string_length = 0;
	for (i = 0; i < t->num_elts; i++) {
		if (t->elements[i].tag == 's') {
			string_length += t->elements[i].data.s.len;
		}
	}
	if (send_chunk(ctx, (char *) &string_length, sizeof(int))) {
		PERROR("send_chunk failed");
		return 1;
	}

	string_length = 0;
	//fprintf(stderr,"Sending %d elts\n", t->num_elts);
	for (i = 0; i < t->num_elts; i++) {
		struct element *e = &t->elements[i];
		//fprintf(stderr,"Sending elt %c\n", e->tag);
		if (e->tag == 's') {
			/*
			 * translate pointers into indexes 
			 */
			if (send_chunk(ctx, (char *) &e->tag, sizeof(int))) {
				PERROR("send_chunk failed");
				return 1;
			}
			if (send_chunk
			    (ctx, (char *) &string_length, sizeof(int))) {
				PERROR("send_chunk failed");
				return 1;
			}
			if (send_chunk
			    (ctx, (char *) &e->data.s.len, sizeof(int))) {
				PERROR("send_chunk failed");
				return 1;
			}
			string_length += e->data.s.len;
		}
		else {
			if (send_chunk
			    (ctx, (char *) e, sizeof(struct element))) {
				PERROR("send_chunk failed");
				return 1;
			}
			if (e->tag == 't') {
				if (send_tuple(ctx, e->data.t)) {
					PERROR("send_tuple failed");
					return 1;
				}
			}
		}
	}

	for (i = 0; i < t->num_elts; i++) {
		if (t->elements[i].tag == 's') {
			if (send_chunk(ctx,
				       (char *) t->elements[i].data.s.
				       ptr, t->elements[i].data.s.len)) {
				PERROR("send_chunk failed");
				return 1;
			}
		}
	}
	return 0;
}


/**
 *
 */
int
recv_chunk(struct context *ctx, char *buf, int size)
{
	int gotten;
	int togo=size;
	/*
	 * The server should not quit if a client dies, But we want
	 * clients to stop running (Broken pipe) if the server stops,
	 * or at least pass an error message up to whoever called
	 * down here.
	 * 
	 * I had intended to use MSG_NOSIGNAL as a mask argument to
	 * recv() for this purpose, but it turns out that the signal
	 * only kills that thread, not the whole server, and that's
	 * exactly the desired behavior.
	 *
	 * Although I don't know if it puts the thread in some wierd
	 * limbo state, leaving the entry in tuple_server.c's
	 * client_list unuseable. This will be something to
	 * investigate as time permits.
	 */
	while (togo>0)
	{
		gotten = recv(ctx->sock, buf+size-togo, togo, MSG_WAITALL);
		if (gotten == 0) {
			// peer did an orderly shutdown
			PRINTF("%s did orderly shutdown. togo=%d, gotten=%d, ctx->id=%d\n", i_am_server ? "Client" : "Server", togo, gotten, ctx->id);
			return 1; // signal failure
		}
		if (gotten < 0) {
			PRINTF("%s broke connection? ctx.id=%d\n", i_am_server ? "Client" : "Server", ctx->id);
			PERROR("recv failed");
			PRINTF("retry result = %d\n", recv(ctx->sock, buf+size-togo, togo, MSG_WAITALL));
			PERROR("retry of recv failed");
			return 1; // signal failure
		}
		if (gotten < togo)
		{
			PRINTF("partial receive: %d of %d bytes\n", gotten, togo);
		}
		togo -= gotten;
	}
	return 0; // signal success
}


/**
 *
 */
struct tuple *
recv_tuple(struct context *ctx)
{
	struct tuple *s;
	int i, num_elts, string_length, element_size;

	if (recv_chunk(ctx, (char *) &num_elts, sizeof(int))) {
		PERROR("recv_chunk failed");
		return NULL;
	}

	/*
	 * return code for non-blocking gets and reads 
	 */
	if (num_elts == -1) {
		return (struct tuple *) -1;
	}

	if (recv_chunk(ctx, (char *) &string_length, sizeof(int))) {
		PERROR("recv_chunk failed");
		return NULL;
	}
	assert(string_length >= 0);

	s = malloc(sizeof(struct tuple));
	if (s == NULL) {
		PERROR("malloc failed");
		EXIT();
	}
	s->num_elts = num_elts;
	s->string_length = string_length;

	element_size = num_elts * sizeof(struct element);
	s->elements = malloc(element_size);
	if (s->elements == NULL) {
		PERROR("malloc failed");
		EXIT();
	}

	// Recursively receive all the elements
	for (i=0; i<num_elts; i++) {
		if (recv_chunk(ctx, (char *) (s->elements+i), sizeof(struct element))) {
			PERROR("recv_chunk failed");
			PRINTF("element %d of %d was not received\n", i, num_elts);
			return NULL;
		}
		if (s->elements[i].tag == 't') {
			struct tuple *nested_tuple = recv_tuple(ctx);
			if (!nested_tuple)
			{
				PRINTF("Could not receive a nested tuple\n");
				return NULL;
			}
			s->elements[i].data.t = nested_tuple;
		}
	}

	if (string_length) {
		s->string_space = malloc(string_length);
		if (s->string_space == NULL) {
			PERROR("malloc failed");
			EXIT();
		}
		if (recv_chunk(ctx, s->string_space, string_length)) {
			PRINTF("recv_chunk failed for string data\n");
			PRINTF("string_length=%d, num_elts=%d, string_space=%p(%s)\n", string_length, s->num_elts, s->string_space, s->string_space);
			return NULL;
		}

		/*
		 * translate index values to pointers 
		 */
		for (i = 0; i < num_elts; i++) {
			if (s->elements[i].tag == 's') {
				int n = (int) s->elements[i].data.s.ptr;
				s->elements[i].data.s.ptr =
					s->string_space + n;
			}
		}
	}
	else {
		s->string_space = NULL;
	}
	return s;
}


/*
 * ----------------------------------------------------------- 
 */

/**
 *
 */
static int
open_client_socket(struct context *ctx)
{
	struct sockaddr_in addr;
	static int context_id=1;
//	int optval=1;

	ctx->id = context_id++;
	ctx->sock = socket(AF_INET, SOCK_STREAM, 0);
	if (ctx->sock < 0) {
		PERROR("Can't create socket");
		return 1;
	}
#if 0
	/*
	 * Set some socket options, so that we have fast response
	 */
	if (setsockopt (ctx->sock, IPPROTO_TCP, TCP_NODELAY, &optval, sizeof (optval))) {
		PERROR("setsockopt failed on TCP_NODELAY");
		return 1;
	}
#endif

	/*
	 * convert server name to dotted quad 
	 */
	struct hostent *he = gethostbyname(ctx->peername);
	if (he == NULL) {
		PERROR("connect failed (bad hostname?)");
		return 1;
	}
	assert(he->h_addrtype == AF_INET);
	
	memset(&addr, 0, sizeof(addr));
	addr.sin_family = AF_INET;
	addr.sin_addr.s_addr = ((struct in_addr *)he->h_addr_list[0])->s_addr;
	addr.sin_port = htons(ctx->portnumber);

	if (connect(ctx->sock, (struct sockaddr *) &addr, sizeof(struct sockaddr_in)) < 0) 
	{
		PERROR("connect failed");
		// When the system is flooded with lingering TIME_WAIT
		// sockets, I hit a EADDRNOTAVAIL here.
		fprintf
		(
			stderr,
			"connection to %d.%d.%d.%d:%d not possible\n",
			(addr.sin_addr.s_addr >>  0) & 0xff,
			(addr.sin_addr.s_addr >>  8) & 0xff,
			(addr.sin_addr.s_addr >> 16) & 0xff,
			(addr.sin_addr.s_addr >> 24) & 0xff,
			ctx->portnumber
		);
		return 1;
	}

	return 0;
}




/**
 *
 */
int
put_tuple(struct tuple *s, struct context *ctx)
{
	int op = PUT;
	if (open_client_socket(ctx)) {
		PERROR("open_client_socket failed");
		return 1;
	}
	if (send_chunk(ctx, (char *) &op, sizeof(int))) {
		PERROR("send failed");
		return 1;
	}
	if (send_tuple(ctx, s)) {
		PERROR("send_tuple failed");
		return 1;
	}
	// Advise peer that we have stopped sending
	SHUTDOWN_SOCKET(ctx->sock, SHUT_WR);
#if 1 // Why is this? - Bram
	if (recv_chunk(ctx, (char *) &op, sizeof(int))) {
		PERROR("recv_chunk failed");
		return 1;
	}
	assert(op == PUT);
#endif
	if (close(ctx->sock))
	{
		PERROR("close failed");
	}
	ctx->id=0;
	ctx->sock=0;
	return 0;
}


/**
 *
 */
int
replace_tuple(struct tuple *template, struct tuple *replacement, struct context *ctx)
{
	int op = REPLACE;
	if (open_client_socket(ctx)) {
		PERROR("open_client_socket failed");
		return 1;
	}
	if (send_chunk(ctx, (char *) &op, sizeof(int))) {
		PERROR("send failed");
		return 1;
	}
	if (send_tuple(ctx, template)) {
		PERROR("send_tuple failed");
		return 1;
	}
	if (send_tuple(ctx, replacement)) {
		PERROR("send_tuple failed");
		return 1;
	}
	// Advise peer that we have stopped sending
	SHUTDOWN_SOCKET(ctx->sock, SHUT_WR);
	// Really necessary?
	if (recv_chunk(ctx, (char *) &op, sizeof(int))) {
		PERROR("recv_chunk failed");
		return 1;
	}
	assert(op == REPLACE);
	if (close(ctx->sock))
	{
		PERROR("close failed");
	}
	ctx->id=0;
	ctx->sock=0;
	return 0;
}


/**
 *
 */
struct tuple *
get_tuple(struct tuple *s, struct context *ctx)
{
	int op = GET;
	if (open_client_socket(ctx)) {
		PERROR("open_client_socket failed");
		return NULL;
	}
	if (send_chunk(ctx, (char *) &op, sizeof(int))) {
		PERROR("send failed");
		return NULL;
	}
	if (send_tuple(ctx, s)) {
		PERROR("send_tuple failed");
		return NULL;
	}
	// Advise peer that we have stopped sending
	SHUTDOWN_SOCKET(ctx->sock, SHUT_WR);
	DBGPRINTF("\n");
	s = recv_tuple(ctx);
	DBGPRINTF("s = %08x\n", (int) s);
	if (s == NULL) {
		PERROR("recv_tuple failed");
		return NULL;
	}
	if (s == (struct tuple *) -1) {
		PERROR("recv_tuple failed");
		return NULL;
	}
	if (close(ctx->sock))
	{
		PERROR("close failed");
	}
	ctx->sock=0;
	ctx->id=0;
	return s;
}


/**
 *
 */
struct tuple *
read_tuple(struct tuple *template_tuple, struct context *ctx)
{
	int op = READ;
        struct tuple *returned_tuple=0;
	if (open_client_socket(ctx)) {
		PERROR("open_client_socket failed");
		return NULL;
	}
	if (send_chunk(ctx, (char *) &op, sizeof(int))) {
		PERROR("send failed");
		return NULL;
	}
	if (send_tuple(ctx, template_tuple)) {
		PERROR("send_tuple failed");
		return NULL;
	}
	// Advise peer that we have stopped sending
	SHUTDOWN_SOCKET(ctx->sock, SHUT_WR);
	DBGPRINTF("\n");
	returned_tuple = recv_tuple(ctx);
	DBGPRINTF("returned_tuple = %08x\n", (int) returned_tuple);
	if (returned_tuple == NULL) {
		PERROR("recv_tuple failed for read");
		print_tuple_stderr(template_tuple);
		fprintf(stderr, " was the template.\n");
		return NULL;
	}
	if (returned_tuple == (struct tuple *) -1) {
		PERROR("recv_tuple failed for read");
		print_tuple_stderr(template_tuple);
		fprintf(stderr, " was the template.\n");
		return NULL;
	}
	if (close(ctx->sock))
	{
		PERROR("close failed");
	}
	ctx->sock=0;
	ctx->id=0;
	return returned_tuple;
}


/**
 *
 */
struct tuple *
get_nb_tuple(struct tuple *template_tuple, struct context *ctx)
{
	int op = GET_NB;
	struct tuple *returned_tuple = 0;
	if (open_client_socket(ctx)) {
		PERROR("open_client_socket failed");
		return (struct tuple *) -1;
	}
	if (send_chunk(ctx, (char *) &op, sizeof(int))) {
		PERROR("send failed");
		return (struct tuple *) -1;
	}
	if (send_tuple(ctx, template_tuple)) {
		PERROR("send_tuple failed");
		return (struct tuple *) -1;
	}
	// Advise peer that we have stopped sending
	SHUTDOWN_SOCKET(ctx->sock, SHUT_WR);
	returned_tuple = recv_tuple(ctx);
	if (returned_tuple == NULL) {
		PERROR("recv_tuple failed for nonblocking get");
		print_tuple_stderr(template_tuple);
		fprintf(stderr, " was the template.\n");
		return (struct tuple *) -1;
	}
	if (returned_tuple == (struct tuple *) -1) {
		if (close(ctx->sock))
		{
			PERROR("close failed");
		}
		ctx->sock=0;
		ctx->id=0;
		return NULL;
	}
	if (close(ctx->sock))
	{
		PERROR("close failed");
	}
	ctx->sock=0;
	ctx->id=0;
	return returned_tuple;
}


/**
 *
 */
struct tuple *
read_nb_tuple(struct tuple *template_tuple, struct context *ctx)
{
	int op = READ_NB;
	struct tuple *received_tuple=0;
	if (open_client_socket(ctx)) {
		PERROR("open_client_socket failed");
		return (struct tuple *) -1;
	}
	if (send_chunk(ctx, (char *) &op, sizeof(int))) {
		PERROR("send failed");
		return (struct tuple *) -1;
	}
	if (send_tuple(ctx, template_tuple)) {
		PERROR("send_tuple failed");
		return (struct tuple *) -1;
	}
	// Advise peer that we have stopped sending
	SHUTDOWN_SOCKET(ctx->sock, SHUT_WR);
	DBGPRINTF("\n");
	received_tuple = recv_tuple(ctx);
	DBGPRINTF("received_tuple = %08x\n", (int) received_tuple);
	if (received_tuple == NULL) {
		PERROR("recv_tuple failed");
		return (struct tuple *) -1;
	}
	if (received_tuple == (struct tuple *) -1) {
		if (close(ctx->sock))
		{
			PERROR("close failed");
		}
		ctx->sock=0;
		ctx->id=0;
		return NULL;
	}
	if (close(ctx->sock))
	{
		PERROR("close failed");
	}
	ctx->sock=0;
	ctx->id=0;
	return received_tuple;
}


/*
 * ----------------------------------------------------------- 
 */



/**
 * Transmit a list of templates to the server, with an op code
 * of DUMP. The server will respond by sending back a list of
 * the matching tuples.
 */
struct tuple_list *
dump_tuple_space(struct tuple_list *templates, struct context *ctx)
{
	int count, op = DUMP;
	struct tuple_list *s, *t, *tlist;

	for (count = 0, s = templates; s != NULL; count++, s = s->next);

	if (open_client_socket(ctx)) {
		PERROR("open_client_socket failed");
		return NULL;
	}

	if (send_chunk(ctx, (char *) &op, sizeof(int))) {
		PERROR("send failed");
		return NULL;
	}
	if (send_chunk(ctx, (char *) &count, sizeof(int))) {
		PERROR("send failed");
		return NULL;
	}
	s = templates;
	while (s) {
		if (send_tuple(ctx, s->tup)) {
			PERROR("send_tuple failed");
			return NULL;
		}
		s = s->next;
	}
	/*
	 * Do NOT free the templates. This guy may want to
	 * use them again later. Let him free them.
	 */

	tlist = NULL;
	if (recv_chunk(ctx, (char *) &count, sizeof(int))) {
		PERROR("recv_chunk failed");
		return NULL;
	}
	while (count--) {
		struct tuple_list *x = malloc(sizeof(struct tuple_list));
		if (x == NULL) {
			/*
			 * ? ? ? ? 
			 */
		}
		x->tup = recv_tuple(ctx);
		DBGPRINTF("x->tup = %08x\n", (int) x->tup);
		if (x->tup == NULL) {
			PERROR("recv_tuple failed");
			return NULL;
		}
		if (x->tup == (struct tuple *) -1) {
			PERROR("recv_tuple failed");
			return NULL;
		}
		x->next = tlist;
		tlist = x;
	}

	DBGPRINTF("tlist = %08x\n", (int) tlist);
	for (t = tlist; t != NULL;) {
		DBGPRINTF("t = %08x\n", (int) t);
		DBGPRINTF("t->tup = %08x\n", (int) t->tup);
		DBGPRINTF("t->next = %08x\n", (int) t->next);
		t = t->next;
	}

	return tlist;
}


/**
 * This works just like dump_tuple_space, but instead of
 * sending back a list of all the matching tuples, we just
 * send back a count of how many tuples match.
 */
int
count_tuple_space(struct tuple_list *templates,
		  struct context *ctx, int *result)
{
	int count, op = COUNT;
	struct tuple_list *s, *tlist;

	for (count = 0, s = templates; s != NULL; count++, s = s->next);

	if (open_client_socket(ctx)) {
		PERROR("open_client_socket failed");
		return 1;
	}

	if (send_chunk(ctx, (char *) &op, sizeof(int))) {
		PERROR("send failed");
		return 1;
	}
	if (send_chunk(ctx, (char *) &count, sizeof(int))) {
		PERROR("send failed");
		return 1;
	}
	s = templates;
	while (s) {
		if (send_tuple(ctx, s->tup)) {
			PERROR("send_tuple failed");
			return 1;
		}
		s = s->next;
	}
	/*
	 * Do NOT free the templates. This guy may want to
	 * use them again later. Let him free them.
	 */

	tlist = NULL;
	if (recv_chunk(ctx, (char *) result, sizeof(int))) {
		PERROR("recv_chunk failed");
		return 1;
	}

	return 0;
}


/**
 *
 */
int
tuple_server_log(FILE * stream, struct context *ctx)
{
	int n, op = LOG;
	const int bufsize = LOGBUFSIZE;
	char buf[bufsize];

	if (open_client_socket(ctx)) {
		PERROR("open_client_socket failed");
		return 1;
	}
	if (send_chunk(ctx, (char *) &op, sizeof(int))) {
		if (close(ctx->sock))
		{
			PERROR("close failed");
		}	
		ctx->sock=0;
		ctx->id=0;
		PERROR("send_chunk failed");
		return 1;
	}

	n = read(ctx->sock, buf, bufsize);
	if (n < 0) {
		PERROR("read failed");
		return 1;
	}
	if (n > 0 && fwrite(buf, 1, n, stream) < n)
		return 0;
	if (close(ctx->sock))
	{
		PERROR("close failed");
	}
	ctx->sock=0;
	ctx->id=0;
	return 0;
}



/*
 * ----------------------------------------------------------- 
 */

/**
 *
 */
static struct element *
tuple_field(struct tuple *s, int n)
{
	if (n >= s->num_elts) {
		fprintf(stderr, "tuple_field() index too big (%d)\n", n);
		fprintf(stderr, "tuple only has %d elements\n", s->num_elts);
		EXIT();
	}
	return &s->elements[n];
}


/**
 *
 */
int
tuple_int_field(struct tuple *s, int n)
{
	struct element *e;
	e = tuple_field(s, n);
	if (e->tag != 'i') {
		fprintf(stderr,
			"tuple_int_field: field %d is not an int\n", n);
		fprintf(stderr, "Here is the tuple\n");
		print_tuple(s);
		logbuf[logptr] = '\0';
		fprintf(stderr, logbuf);
		EXIT();
	}
	return e->data.i;
}


/**
 *
 */
double
tuple_double_field(struct tuple *s, int n)
{
	struct element *e;
	e = tuple_field(s, n);
	if (e->tag != 'd') {
		fprintf(stderr,
			"tuple_double_field: field %d is not a double\n", n);
		fprintf(stderr, "Here is the tuple\n");
		print_tuple(s);
		logbuf[logptr] = '\0';
		fprintf(stderr, logbuf);
		EXIT();
	}
	return e->data.d;
}


/**
 *
 */
char *
tuple_string_field(struct tuple *s, int *len, int n)
{
	struct element *e;
	e = tuple_field(s, n);
	if (e->tag != 's') {
		fprintf(stderr,
			"tuple_string_field: field %d is not a string\n", n);
		fprintf(stderr, "Here is the tuple\n");
		print_tuple(s);
		logbuf[logptr] = '\0';
		fprintf(stderr, logbuf);
		EXIT();
	}
	if (len != NULL)
		*len = e->data.s.len;
	return e->data.s.ptr;
}


/**
 *
 */
struct tuple *
tuple_tuple_field(struct tuple *s, int n)
{
	struct element *e;
	e = tuple_field(s, n);
	if (e->tag != 't') {
		fprintf(stderr,
			"tuple_tuple_field: field %d is not a tuple\n", n);
		fprintf(stderr, "Here is the tuple\n");
		print_tuple(s);
		logbuf[logptr] = '\0';
		fprintf(stderr, logbuf);
		EXIT();
	}
	return e->data.t;
}


/*
 * ----------------------------------------------------------- 
 */
