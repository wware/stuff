/* igobjs.c -- C code, objects for Iguana
 * (c)2000 Will Ware
 */

#include <math.h>
#include <stdio.h>
#include "Python.h"
#include "igobjs.h"
#include "ops.h"

#define CURIOUS(x)

enum error_type
{
  ERR_NONE, ERR_RUNTIME, ERR_INDEX,
};

static char error_string[256];
static int error_status = ERR_NONE;

PyObject *
python_croak (void)
{
  switch (error_status)
    {
    case ERR_RUNTIME:
      PyErr_SetString (PyExc_RuntimeError, error_string);
      return 0;
    case ERR_INDEX:
      PyErr_SetString (PyExc_IndexError, error_string);
      return 0;
    }
  PyErr_SetString (PyExc_RuntimeError, "clueless programmer error");
  return 0;
}

void
clear_exception (void)
{
  error_status = 0;
}

void
throw_exception (enum error_type et, char *msg)
{
  strncpy (error_string, msg, 256);
  error_status = et;
}

int
check_exception (void)
{
  return error_status;
}

/* =============== Program stuff =========== */

Program *
new_Program (void)
{
  Program *self;
  self = (Program *) malloc (sizeof (Program));
  self->compileptr = self->startaddr = 0;
  return self;
}

void
Program_compile (Program * self, int x)
{
  if (self->compileptr < ROMSIZE)
    {
      self->data[self->compileptr] = x;
      self->compileptr++;
    }
  else
    throw_exception (ERR_INDEX, "Program index out of bounds");
}

void
Program_compile_double (Program * self, double x)
{
  if (self->compileptr < ROMSIZE - 1)
    {
      /* I'm cheating with my knowledge that on this platform, a double is
       * 2 times the size of an int. I should make it more portable.
       */
      *((double*) &(self->data[self->compileptr])) = x;
      self->compileptr += 2;
    }
  else
    throw_exception (ERR_INDEX, "Program index out of bounds");
}

double
Program_read_const (Program * self, unsigned int n)
{
  double x = 0.0;
  if (n < ROMSIZE - 1)
    {
      /* I'm cheating with my knowledge that on this platform, a double is
       * 2 times the size of an int. I should make it more portable.
       */
      x = *((double*) &(self->data[n]));
    }
  else
    throw_exception (ERR_INDEX, "Program index out of bounds");
  return x;
}

unsigned int
Program___getitem__ (Program * self, unsigned int n)
{
  if (n < ROMSIZE)
    return self->data[n];
  throw_exception (ERR_INDEX, "Program index out of bounds");
  return 0;
}

void
Program___setitem__ (Program * self, int n, unsigned int x)
{
  if (n < ROMSIZE)
    self->data[n] = x;
  else
    throw_exception (ERR_INDEX, "Program index out of bounds");
}




/* =============== Thread stuff =========== */

Thread *
new_Thread (Program * myprog)
{
  Thread *self;
  if (myprog == 0)
    return 0;
  self = (Thread *) malloc (sizeof (Thread));
  CURIOUS (printf ("new Thread at %x\n", self));
  self->myProgram = myprog;
  self->dsptr = self->rsptr = self->stopped = self->instrucptr = 0;
  return self;
}

void
delete_Thread (Thread * self)
{
  CURIOUS (printf ("delete Thread at %x\n", self));
  free (self);
}

double
Thread___getitem__ (Thread * self, unsigned int n)
{
  double x;
  if (n < STACKDEPTH)
    {
      x = self->data_stack[n];
      return x;
    }
  throw_exception (ERR_INDEX, "Thread index out of bounds");
  return 0.0;
}

void
Thread___setitem__ (Thread * self, int n, double x)
{
  if (n < STACKDEPTH)
    self->data_stack[n] = x;
  else
    throw_exception (ERR_INDEX, "Thread index out of bounds");
}

double
Thread_fetchvar (Thread * self, unsigned int n)
{
  double x;
  if (n < NUMVARS)
    {
      x = self->variables[n];
      return x;
    }
  throw_exception (ERR_INDEX, "Thread var index out of bounds");
  return 0.0;
}

void
Thread_storevar (Thread * self, int n, double x)
{
  if (n < NUMVARS)
    self->variables[n] = x;
  else
    throw_exception (ERR_INDEX, "Thread var index out of bounds");
}

int
Thread_pop (Thread * self)
{
  int x;
  if (self->dsptr == 0)
    {
      throw_exception (ERR_INDEX, "Thread pop underflow");
      return 0.0;
    }
  x = self->data_stack[--self->dsptr];
  return x;
}

void
Thread_push (Thread * self, int x)
{
  if (self->dsptr == STACKDEPTH - 1)
    {
      throw_exception (ERR_INDEX, "Thread push overflow");
      return;
    }
  self->data_stack[self->dsptr++] = x;
}

double
Thread_double_pop (Thread * self)
{
  double x;
  if (self->dsptr == 0)
    {
      throw_exception (ERR_INDEX, "Thread pop underflow");
      return 0.0;
    }
  self->dsptr -= 2;
  return *((double*) &(self->data_stack[self->dsptr]));
}

void
Thread_double_push (Thread * self, double x)
{
  if (self->dsptr == STACKDEPTH - 1)
    {
      throw_exception (ERR_INDEX, "Thread push overflow");
      return;
    }
  *((double*) &(self->data_stack[self->dsptr])) = x;
  self->dsptr += 2;
}

void
Thread_stack_dump (Thread *self)
{
  unsigned int i;
  printf("[ ");
  for (i = 0; i < self->dsptr; i++)
    printf("%d ", self->data_stack[i]);
  printf("]\n");
}

void
Thread_steps (Thread * self, int n)
{
  unsigned int *prog = self->myProgram->data;
  int next_instruc;
  while (n--)
    {
      CURIOUS(Thread_stack_dump(self));
      if (self->stopped)
	return;
      if (self->instrucptr >= ROMSIZE)
	{
	  throw_exception (ERR_RUNTIME, "overran program memory");
	  return;
	}
      next_instruc = prog[self->instrucptr];
      CURIOUS (printf ("instrucptr=%d, next_instruc=%d\n",
		       self->instrucptr, next_instruc));
      self->instrucptr++;
      /* refer to one of the primitives */
      if (next_instruc < 0)
	{
	  switch (next_instruc)
	    {
#include "ops.c"
	    default:
	      sprintf (error_string, "unknown opcode %d", next_instruc);
	      throw_exception (ERR_RUNTIME, error_string);
	      return;
	    }
	}
      else
	{
	  if (self->rsptr == STACKDEPTH - 1)
	    {
	      throw_exception (ERR_INDEX, "Thread return push overflow");
	      return;
	    }
	  self->return_stack[self->rsptr++] = self->instrucptr;
	  self->instrucptr = next_instruc;
	}
    }
}

void
Thread_step (Thread * self)
{
  Thread_steps (self, 1);
}

void
Thread_run (Thread * self)
{
  self->stopped = 0;
  self->instrucptr = self->myProgram->startaddr;
  while (!self->stopped)
    Thread_steps (self, 10000);
}
