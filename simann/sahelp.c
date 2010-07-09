/* Simulated annealing helper
 */

#include "Python.h"

// it's deterministic, but it will do well enough
static int x=11, y=13, z=17;

static Py_ssize_t randint(Py_ssize_t N)
{
    x = (171 * x) % 30269;
    y = (172 * y) % 30307;
    z = (170 * z) % 30323;
    double r = x/30269.0 + y/30307.0 + z/30323.0;
    while (r > 1.0)
	r -= 1.0;
    return (Py_ssize_t) (r * N);
}

static PyObject *
sahelp_shuffle(PyObject *self, PyObject *args)
{
    PyObject *lst;
    Py_ssize_t j, k, n, N;
    if (!PyArg_ParseTuple(args, "Oi", &lst, &n))
	return NULL;
    if (!PyList_Check(lst)) {
	PyErr_BadArgument();
	return NULL;
    }
    N = PyList_Size(lst);
    while (n--) {
	j = randint(N);
	k = randint(N);
	if (j != k) {
	    PyObject *p, *q;
	    p = PyList_GetItem(lst, j);
	    q = PyList_GetItem(lst, k);
	    Py_INCREF(p);
	    Py_INCREF(q);
	    PyList_SetItem(lst, k, p);
	    PyList_SetItem(lst, j, q);
	}
    }
    Py_INCREF(Py_None);
    return Py_None;
}

static PyMethodDef
sahelp_methods[] = {
    {"shuffle", (PyCFunction)sahelp_shuffle, METH_VARARGS},
    {NULL, NULL}
};

void
initsahelp(void)
{
    PyImport_AddModule("sahelp");
    Py_InitModule("sahelp", sahelp_methods);
}

/*
 * Local Variables:
 * c-basic-offset: 4
 * tab-width: 8
 * End:
 */
