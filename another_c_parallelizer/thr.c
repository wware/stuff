#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>
#include <string.h>

#include "thr.h"

struct thread *current_thread = NULL;

#ifdef DEBUG
struct symbol_table_link {
	char *name;
	void *p;
	struct symbol_table_link *next;
};

static struct symbol_table_link *symbol_table = NULL;
static int got_basics_registered = 0;

static int indent_level = 0;

void indent_func(void)
{
	int i;
	for (i = 0; i < indent_level; i++)
		fprintf(stderr, "  ");
}

void enter_thingy(char *filename, int linenum, char *funcname)
{
	indent_func();
	fprintf(stderr,
		"%s:%d entering %s\n", filename, linenum, funcname);
	indent_level++;
}

void leave_thingy(char *filename, int linenum, char *funcname)
{
	indent_level--;
	indent_func();
	fprintf(stderr, "%s:%d leaving %s\n", filename, linenum, funcname);
}


void register_symbol(char *name, void *p)
{
	struct symbol_table_link *link;
	link = malloc(sizeof(struct symbol_table_link));
	link->name = malloc(strlen(name) + 1);
	strcpy(link->name, name);
	link->p = p;
	link->next = symbol_table;
	symbol_table = link;
}

char *lookup_symbol(void *p)
{
	struct symbol_table_link *link;
	if (!got_basics_registered) {
		register_symbol("pfork", pfork);
		register_symbol("pexit", pexit);
		got_basics_registered = 1;
	}
	for (link = symbol_table; link; link = link->next)
		if (link->p == p)
			return link->name;
	return "?????";
}

void display_frame(char *preamble, void *addr, struct generic_frame *frame)
{
	indent_func();
	fprintf(stderr, "%s %08X %s %d\n",
		preamble, (int) addr,
		lookup_symbol(frame->func), frame->tag);
}


#define DISPLAY_THREAD(T) \
    _DBG_(display_thread(T, #T, __FILE__, __LINE__))

void
display_thread(struct thread *T, char *threadname,
	       char *filename, int linenum)
{
	struct generic_frame *f;
	indent_func();
	fprintf(stderr, "%s:%d Thread: %s [%08X]\n",
		filename, linenum, threadname, (int) T);
	if (T) {
		indent_level++;
		for (f = T->frame; f != NULL; f = f->caller)
			display_frame("Frame", f, f);
		indent_level--;
	}
}
#else
#define display_frame(preamble, addr, f)
#define DISPLAY_THREAD(T)
#define display_thread(T)
#endif


/* copy_thread is used for fork */
static struct thread *copy_thread(struct thread *old)
{
	struct thread *newthread;
	struct generic_frame *oldframe, *newframe = NULL;
	struct generic_frame *previous_newframe = NULL;
	struct generic_frame *previous_oldframe = NULL;
	newthread = new_thread(NULL, old->frame->framesize);
	for (oldframe = old->frame;
	     oldframe != NULL; oldframe = oldframe->caller) {
		/* Clone the old frame, make a copy of it for the new thread.
		 */
		newframe = malloc(oldframe->framesize);
		if (newframe == NULL) {
			fprintf(stderr, "ouch, out of memory\n");
			exit(1);
		}
		memcpy(newframe, oldframe, oldframe->framesize);
		/* Clean up the context and caller fields, but all locals and
		 * arguments should be exact copies. return_value_pointer which
		 * points into the caller's frame must be correctly rewritten
		 * for the new locations.
		 */
		newframe->context = newthread;
		if (previous_newframe == NULL)
			newthread->frame = newframe;
		else {
			if (previous_oldframe->return_value_pointer !=
			    NULL) {
				char *pofr =
				    (char *) previous_oldframe->
				    return_value_pointer;
				char *pofc =
				    (char *) previous_oldframe->caller;
				previous_newframe->return_value_pointer =
				    (((char *) newframe) + (pofr - pofc));
			} else
				previous_newframe->return_value_pointer =
				    NULL;
			previous_newframe->caller = newframe;
		}
		previous_newframe = newframe;
		previous_oldframe = oldframe;
	}
	newframe->caller = NULL;
	return newthread;
}

int pfork(struct thread *context)
{
	struct pfork_frame *frame = (struct pfork_frame *) context->frame;

	if (frame->tag == 1)
		goto L1;

	frame->tag = 1;
	frame->new = 0;
	frame->new = copy_thread(current_thread);
	DISPLAY_THREAD(current_thread);
	DISPLAY_THREAD(frame->new);
	return 0;

      L1:;
	*((struct thread **) frame->return_value_pointer) = frame->new;
	return 1;
}


struct generic_frame *new_frame(int (*func) (struct thread *),
				int framesize, struct thread *context)
{
	struct generic_frame *p;
	p = malloc(framesize);
	if (p == NULL) {
		fprintf(stderr, "ouch, out of memory\n");
		exit(1);
	}
	p->func = func;
	p->tag = 0;
	ASSERT(context);
	p->context = context;
	p->caller = context->frame;
	p->framesize = framesize;
	context->frame = p;
	return p;
}


struct thread *new_thread(int (*func) (struct thread *), int framesize)
{
	struct thread *r;
	r = malloc(sizeof(struct thread));
	if (r == NULL) {
		fprintf(stderr, "ouch, out of memory\n");
		exit(1);
	}
	if (current_thread == NULL) {
		r->prev = r->next = r;
		r->frame = NULL;
		current_thread = r;
	} else {
		ASSERT(current_thread);
		ASSERT(current_thread->next);
		r->prev = current_thread;
		r->next = current_thread->next;
		current_thread->next->prev = r;
		current_thread->next = r;
	}
	r->frame = NULL;
	r->frame = new_frame(func, framesize, r);
	ASSERT(r->frame);
	return r;
}


int pexit(struct thread *context)
{
	/* This thing all things devours */
	struct generic_frame *p, *q;
	ENTER();
	p = current_thread->frame->caller;
	while (p != NULL) {
		q = p->caller;
		free(p);
		p = q;
	}
	current_thread->frame->caller = NULL;
	LEAVE();
	return 1;
}


static int one_step(void)
{
	if (current_thread == NULL)
		return 1;
	if (current_thread->frame != NULL) {
		display_frame("Step", current_thread,
			      current_thread->frame);
		if ((current_thread->frame->func) (current_thread)) {
			struct generic_frame *old_frame;
			DBGPRINTF1("%s finished\n",
				   lookup_symbol(current_thread->frame->
						 func));
			old_frame = current_thread->frame;
			current_thread->frame =
			    current_thread->frame->caller;
			free(old_frame);
		}
		/* if this thread has finished running, snip it from
		 * the linked list
		 */
		if (current_thread->frame == NULL) {
			struct thread *before, *after, *oldthread;
			if (current_thread == current_thread->next) {
				/*
				 * There's only one thread left, so free
				 * it and move on with your life.
				 */
				free(current_thread);
				current_thread = NULL;
				return 1;
			}
			oldthread = current_thread;
			before = current_thread->prev;
			after = current_thread->next;
			ASSERT(before);
			ASSERT(after);
			before->next = after;
			after->prev = before;
			current_thread = after;
			free(oldthread);
		}
	}
	return 0;
}

int step_thread(int n)
{
	while (n--) {
		if (one_step()) {
			return 1;
		}
		ASSERT(current_thread);
		current_thread = current_thread->next;
	}
	return 0;
}

int step_thread_noisy(int n)
{
	while (n--) {
		/* Threads have about 1 chance in 32 of missing a
		 * step. This can be used to model random variations
		 * in clock speed or the like.
		 */
		if (rand() & 0x01F)
			if (one_step())
				return 1;
		ASSERT(current_thread);
		current_thread = current_thread->next;
	}
	return 0;
}
