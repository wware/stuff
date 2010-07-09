/*
 * Genetic algorithm accelerator for Python.
 * (c)2000 Will Ware
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <time.h>
#include <math.h>
#include <stdio.h>
#include "Python.h"
#include "ga.h"

#if defined(Py_DEBUG) || defined(DEBUG)
extern void _Py_CountReferences(FILE*);
#define CURIOUS(x) { fprintf(stderr, __FILE__ ":%d ", __LINE__); x; }
#else
#define CURIOUS(x)
#endif
#define MARKER()        CURIOUS(fprintf(stderr, "\n"))
#define DESCRIBE(x)     CURIOUS(fprintf(stderr, "  " #x "=%d\n", x))
#define DESCRIBE_HEX(x) CURIOUS(fprintf(stderr, "  " #x "=%08x\n", x))
#define COUNTREFS()     CURIOUS(_Py_CountReferences(stderr))

struct sortableItem {
  double fitness;
  PyObject *chr;
};

enum error_type
{
  ERR_NONE, ERR_RUNTIME, ERR_INDEX, ERR_MEMORY,
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
    case ERR_MEMORY:
      PyErr_SetString (PyExc_MemoryError, error_string);
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

static int seedx = 0, seedy = 0, seedz = 0;

#define ENABLE_ASSERTS 1

#if ENABLE_ASSERTS
#define ASSERT(condition) \
  if (!(condition)) { \
    fprintf(stderr, __FILE__ ":%d: Assertion failed\n%s\n", __LINE__, #condition); \
         exit(1); }
#else
#define ASSERT(condition)
#endif

#define GENERICMALLOC(ptr,type,n,retval) \
  ptr = (type *) malloc ((n) * sizeof (type)); \
  if (ptr == NULL) \
    { \
      throw_exception (ERR_MEMORY, "out of memory"); \
      return retval; \
    }
#define MALLOC(ptr,type,n) \
  GENERICMALLOC(ptr,type,n,NULL)
#define VOIDMALLOC(ptr,type,n) \
  GENERICMALLOC(ptr,type,n,)

/* This random number generator is based on the random()
 * function in /usr/lib/python1.5/whrandom.py.
 */
static double drand(void)
{
  double r;
 try_again:
  if (seedx == 0 && seedy == 0 && seedz == 0)
    {
      int i;
      time_t T;
      time(&T);
      /* as of the year 2000, there are about 30 bits of entropy
       * available from time()
       */
      seedx = T & 0x3FF; T >>= 10;
      seedy = T & 0x3FF; T >>= 10;
      seedz = T & 0x3FF;
      for (i = 0; i < 5; i++)
	{
	  seedx = (171 * seedx) % 30269;
	  seedy = (172 * seedy) % 30307;
	  seedz = (170 * seedz) % 30323;
	}
    }
  seedx = (171 * seedx) % 30269;
  seedy = (172 * seedy) % 30307;
  seedz = (170 * seedz) % 30323;
  r = (1.0 * seedx) / 30269;
  r += (1.0 * seedy) / 30307;
  r += (1.0 * seedz) / 30323;
  while (r >= 1.0) r -= 1.0;
  if (isnan(r))
    goto try_again;
  return r;
}

static unsigned int irand(unsigned int x)
{
  return (unsigned int) (x * drand());
}

static void randArray(double *array, unsigned int size, double maxval)
{
  int i;
  for (i = 0; i < size; i++)
    array[i] = maxval * (2 * drand() - 1);
}


/* =============== Chromosome stuff =========== */

Chromosome *
new_Chromosome (int size, int maxval, int randomize)
{
  Chromosome *self;
  self = (Chromosome *) malloc (sizeof (Chromosome));
  if (self == NULL)
    {
      throw_exception (ERR_MEMORY, "out of memory");
      return NULL;
    }
  self->size = size;
  self->maxval = maxval;
  self->letters = (unsigned int *) malloc (size * sizeof (unsigned int));
  if (self->letters == NULL)
    {
      free(self);
      throw_exception (ERR_MEMORY, "out of memory");
      return NULL;
    }
  if (randomize)
    {
      int i;
      for (i = 0; i < size; i++)
	self->letters[i] = irand(maxval);
    }
  /* otherwise no guarantee about what the data is */
  CURIOUS (fprintf (stderr, "new Chromosome at %x\n", self));
  return self;
}

void
delete_Chromosome (Chromosome * self)
{
  CURIOUS (fprintf (stderr, "delete Chromosome at %x\n", self));
  free (self->letters);
  free (self);
}

double
Chromosome___getitem__ (Chromosome * self, unsigned int n)
{
  if (n < self->size)
    return self->letters[n];
  throw_exception (ERR_INDEX, "Chromosome index out of bounds");
  return 0;
}

void
Chromosome___setitem__ (Chromosome * self, unsigned int n, unsigned int x)
{
  if (n < self->size)
    self->letters[n] = x;
  else
    throw_exception (ERR_INDEX, "Chromosome index out of bounds");
}

Chromosome *
Chromosome_cross (Chromosome * self, Chromosome * other)
{
  Chromosome *ret;
  unsigned int crossover;
  if (self->size != other->size || self->maxval != other->maxval)
    {
      throw_exception (ERR_RUNTIME,
		       "Chromosomes only cross with others of same type");
      return NULL;
    }
  ret = new_Chromosome (self->size, self->maxval, 0);
  crossover = irand(self->size);
  if (irand(2))
    {
      memcpy(ret->letters, self->letters, crossover * sizeof(unsigned int));
      memcpy(ret->letters + crossover,
	     other->letters + crossover,
	     (self->size - crossover) * sizeof(unsigned int));
    }
  else
    {
      memcpy(ret->letters, other->letters, crossover * sizeof(unsigned int));
      memcpy(ret->letters + crossover,
	     self->letters + crossover,
	     (self->size - crossover) * sizeof(unsigned int));
    }
  return ret;
}

void
Chromosome_mutate (Chromosome * self, double probability)
{
  unsigned int i, n, x;
  const int numbits = 5;
  for (i = 0; i < numbits; i++)
    if (drand() < probability / numbits)
      {
	n = irand(self->size);
	x = irand(self->maxval);
	self->letters[n] = x;
      }
}

PyObject *
Chromosome_list(Chromosome *self)
{
  int i;
  PyObject *ret;
  ret = PyList_New(self->size);
  if (self == NULL)
    {
      throw_exception (ERR_MEMORY, "out of memory");
      return NULL;
    }
  for (i = 0; i < self->size; i++)
    {
      PyObject *k;
      k = PyInt_FromLong(self->letters[i]);
      PyList_SetItem(ret, i, k);
    }
  return ret;
}

PyObject *
Chromosome_tuple(Chromosome *self)
{
  int i;
  PyObject *ret;
  ret = PyTuple_New(self->size);
  if (self == NULL)
    {
      throw_exception (ERR_MEMORY, "out of memory");
      return NULL;
    }
  for (i = 0; i < self->size; i++)
    {
      PyObject *k;
      k = PyInt_FromLong(self->letters[i]);
      PyTuple_SetItem(ret, i, k);
    }
  return ret;
}

static int chromosome_compare(const void *x, const void *y)
{
  struct sortableItem *gx, *gy;
  gx = (struct sortableItem *) x;
  gy = (struct sortableItem *) y;
  if (gx->fitness < gy->fitness)
    return -1;
  if (gx->fitness > gy->fitness)
    return 1;
  return 0;
}

void
sort_by_fitness (PyObject *lst, PyObject *fitness_function)
{
  int i, size;
  struct sortableItem *data;
  if (!PyList_Check(lst))
    {
      throw_exception (ERR_RUNTIME, "bad 1st arg");
      return;
    }
  size = PyList_Size(lst);
  if (!PyFunction_Check(fitness_function))
    {
      throw_exception (ERR_RUNTIME, "bad 2nd arg");
      return;
    }
  data = (struct sortableItem *) malloc (size * sizeof (struct sortableItem));
  if (data == NULL)
    {
      throw_exception (ERR_MEMORY, "out of memory");
      return;
    }
  for (i = 0; i < size; i++)
    {
      PyObject *arg, *ret, *chr;
      chr = PyList_GetItem(lst, i);
      Py_INCREF(chr);
      data[i].chr = chr;
      arg = Py_BuildValue("(O)", chr);
      ret = PyEval_CallObject(fitness_function, arg);
      Py_DECREF(arg);
      if (ret == NULL || PyErr_Occurred())
	return;
      else if (PyFloat_Check(ret))
	{
	  data[i].fitness = PyFloat_AsDouble(ret);
	  Py_DECREF(ret);
	}
      else if (PyInt_Check(ret))
	{
	  data[i].fitness = (double) PyInt_AsLong(ret);
	  Py_DECREF(ret);
	}
      else
	{
	  throw_exception (ERR_RUNTIME,
			   "fitness function must return a number");
	  return;
	}
    }
  qsort(data, size, sizeof (struct sortableItem), chromosome_compare);
  for (i = 0; i < size; i++)
    PyList_SetItem(lst, i, data[i].chr);
  free(data);
}
