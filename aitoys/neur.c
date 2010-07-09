#include <stdlib.h>
#include <math.h>
#include <time.h>

#include "Python.h"

#define ATAN_FACTOR 10.0

static PyObject *ErrorObject;
static int rand_initialized = 0;

struct singleInput {
  double weight;
  struct sNeurObject *neuron;
};

typedef struct sNeurObject {
  PyObject_HEAD
  double offset, sum, output;
  int numinputs;
  struct singleInput inputs[1];
} NeurObject;

staticforward PyTypeObject Neur_Type;

#define NeurObject_Check(v) ((v)->ob_type == &Neur_Type)

static NeurObject *
newNeurObject(PyObject *inputs)
{
  int i, n;
  NeurObject *self;
  if (!rand_initialized)
    {
      time_t T;
      time(&T);
      srand(T);
    }
  n = PyList_Size(inputs);
  self = (NeurObject *) malloc(sizeof(NeurObject) +
			       (n - 1) * sizeof(struct singleInput));
  self->ob_type = &Neur_Type;
  self->ob_refcnt = 1;
  if (self == NULL)
    return NULL;
  self->offset = (1.0 *rand()/(RAND_MAX+1.0)) - 0.5;
  self->sum = self->output = 0.0;
  self->numinputs = n;
  for (i = 0; i < n; i++)
    {
      self->inputs[i].weight = (1.0 *rand()/(RAND_MAX+1.0)) - 0.5;
      self->inputs[i].neuron = (NeurObject*) PyList_GetItem(inputs, i);
      Py_INCREF(self->inputs[i].neuron);
    }
  return self;
}

/* Neur methods */

static void
Neur_dealloc(self)
     NeurObject *self;
{
  int i;
  for (i = 0; i < self->numinputs; i++)
    {
      Py_DECREF(self->inputs[i].neuron);
    }
  PyMem_DEL(self);
}

static PyObject *
Neur_force(self, args)
     NeurObject *self;
     PyObject *args;
{
  if (!PyArg_ParseTuple(args, "d", &self->output))
    return NULL;
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
Neur_iterate(self, args)
     NeurObject *self;
     PyObject *args;
{
  int i;
  double sum;
  if (self->numinputs > 0)
    {
      if (!PyArg_ParseTuple(args, ""))
	return NULL;
      sum = self->offset;
      for (i = 0; i < self->numinputs; i++)
	{
	  struct singleInput *s;
	  s = &self->inputs[i];
	  sum += s->weight * s->neuron->output;
	}
      self->sum = sum;
      self->output = atan(ATAN_FACTOR * sum);
    }
  Py_INCREF(Py_None);
  return Py_None;
}

static void
internal_adjust(self, delta, learningConstant)
     NeurObject *self;
     double delta, learningConstant;
{
  int i;
  double x;
  x = self->sum * ATAN_FACTOR;
  delta *= ATAN_FACTOR / (1.0 + x * x);
  self->offset += learningConstant * delta;
  for (i = 0; i < self->numinputs; i++)
    {
      struct singleInput *s;
      double w;
      s = &self->inputs[i];
      w = s->weight;
      s->weight = w + learningConstant * delta * s->neuron->output;
      internal_adjust(s->neuron, w * delta, learningConstant);
    }
}

static PyObject *
Neur_adjust(self, args)
     NeurObject *self;
     PyObject *args;
{
  double delta = 0.0, learningConstant = 0.0;
  if (!PyArg_ParseTuple(args, "dd", &delta, &learningConstant))
    return NULL;
  internal_adjust(self, delta, learningConstant);
  Py_INCREF(Py_None);
  return Py_None;
}

static PyMethodDef neuro_methods[] = {
  {"force", (PyCFunction)Neur_force, 1},
  {"iterate", (PyCFunction)Neur_iterate, 1},
  {"adjust", (PyCFunction)Neur_adjust, 1},
  {NULL,  NULL}  /* sentinel */
};

static PyObject *
neuro_getattr(NeurObject *self, char *name)
{
  if (strcmp(name, "out") == 0)
    return Py_BuildValue("d", self->output);
  if (strcmp(name, "sum") == 0)
    return Py_BuildValue("d", self->sum);
  /* handle inputs? weights? */
  return Py_FindMethod(neuro_methods, (PyObject *)self, name);
}


statichere PyTypeObject Neur_Type = {
  /* The ob_type field must be initialized in the module init function
   * to be portable to Windows without using C++. */
  PyObject_HEAD_INIT(NULL)
  0,   /*ob_size*/
  "Neur",   /*tp_name*/
  sizeof(NeurObject), /*tp_basicsize*/
  0,   /*tp_itemsize*/
  /* methods */
  (destructor)Neur_dealloc, /*tp_dealloc*/
  0,   /*tp_print*/
  (getattrfunc)neuro_getattr,   /*tp_getattr*/
  0,   /*tp_setattr*/
  0,   /*tp_compare*/
  0,   /*tp_repr*/
  0,   /*tp_as_number*/
  0,   /*tp_as_sequence*/
  0,   /*tp_as_mapping*/
  0,   /*tp_hash*/
};
/* --------------------------------------------------------------------- */

/* Function of no arguments returning new Neur object */

static PyObject *
neur_new(self, args)
     PyObject *self; /* Not used */
     PyObject *args;
{
  PyObject *inputs;
  NeurObject *rv;
  if (!PyArg_ParseTuple(args, "O", &inputs))
    return NULL;
  if (!PyList_Check(inputs))
    {
      fprintf(stderr, "ouch\n");
      exit(1);
    }
  rv = newNeurObject(inputs);
  if ( rv == NULL )
    return NULL;
  return (PyObject *)rv;
}


/* List of functions defined in the module */

static PyMethodDef neur_methods[] = {
  {"new",  neur_new,  1},
  {NULL,  NULL}  /* sentinel */
};


/* Initialization function for the module (*must* be called initneur) */

DL_EXPORT(void)
     initneur()
{
  PyObject *m, *d;
  
  /* Initialize the type of the new type object here; doing it here
   * is required for portability to Windows without requiring C++. */
  Neur_Type.ob_type = &PyType_Type;
  
  /* Create the module and add the functions */
  m = Py_InitModule("neur", neur_methods);
  
  /* Add some symbolic constants to the module */
  d = PyModule_GetDict(m);
  ErrorObject = PyErr_NewException("neur.error", NULL, NULL);
  PyDict_SetItemString(d, "error", ErrorObject);
}
