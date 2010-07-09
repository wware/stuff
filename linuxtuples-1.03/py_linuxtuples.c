/*****************************************************************
 *
 * LinuxTuples - a Linda-esque tuple system for Linux clusters
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
/**
 * \file py_linuxtuples.c
 * \brief Python API for tuple space clients.
 *
 * The best documentation for this is to look at the examples
 * provided in the Python source code for how to extend the language
 * with modules and objects. The 1.5.2 codebase is easy to read;
 * look for xxobject.c and xxmodule.c.
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <sys/time.h>

#include "Python.h"
#include "tuple.h"


staticforward PyTypeObject CtxType;

/**
 * \brief Python object, wrapper for a struct context.
 */
typedef struct
{
	PyObject_HEAD;
	struct context ctx;
}
CtxObject;


static char ctx_put_doc_string[];
static char ctx_replace_doc_string[];
static char ctx_get_doc_string[];
static char ctx_read_doc_string[];
static char ctx_get_nb_doc_string[];
static char ctx_read_nb_doc_string[];
static char ctx_dump_doc_string[];
static char ctx_count_doc_string[];
static char ctx_log_doc_string[];
static PyMethodDef linuxtuples_methods[];
static PyMethodDef ctx_methods[];

static PyObject *ctx_getattr(CtxObject * obj, char *name);
static PyObject *new_ctx(PyObject * self, PyObject * args);
static void ctx_dealloc(PyObject * self);
static struct tuple *PyTuple_to_LinuxTuple(PyObject * pytup, int puttable);
static PyObject *LinuxTuple_to_PyTuple(struct tuple *t);
static PyObject *ctx_put_tuple(PyObject * self, PyObject * args);
static PyObject *ctx_replace_tuple(PyObject * self, PyObject * args);
static PyObject *ctx_get_read(CtxObject * pyctx,
			      PyObject * args,
			      struct tuple *(*func) (struct tuple *,
						     struct context *),
			      int blocking);
static PyObject *ctx_get_tuple(PyObject * self, PyObject * args);
static PyObject *ctx_read_tuple(PyObject * self, PyObject * args);
static PyObject *ctx_get_nb_tuple(PyObject * self, PyObject * args);
static PyObject *ctx_read_nb_tuple(PyObject * self, PyObject * args);
static PyObject *ctx_dump_tuple_space(PyObject * self, PyObject * args);
static PyObject *ctx_count_tuple_space(PyObject * self, PyObject * args);
static PyObject *ctx_tuple_server_log(PyObject * self, PyObject * args);

static char py_random_int_doc_string[];
static PyObject *py_random_int(PyObject * self, PyObject * args);

void initlinuxtuples(void);


static char
	linuxtuples_doc_string[] =
	"This is the Python client API for the linuxtuples tuple server. The\n"
	"connect() method returns a connection to the tuple server. The\n"
	"random() method returns a 32-bit integer taken from /dev/urandom.";


static char
	ctx_doc_string[] =
	"This is a connection to a tuple server. Its methods give access to\n"
	"tuple space.\n"
	"\n"
	"The put(tuple) method lets you put a tuple into the tuple space.\n"
	"get(template) removes a tuple that matches the template (where None is\n"
	"used as a wildcard), blocking until a matching tuple becomes\n"
	"available. read(template) is like get(), but it returns a copy of the\n"
	"tuple, leaving the original in tuple space.\n"
	"\n"
	"get_nonblocking() and read_nonblocking() are non-blocking versions of\n"
	"get() and read(), which return None if a matching tuple is not\n"
	"available.\n"
	"\n"
	"The dump() method returns a list of the tuples in tuple space. If\n"
	"supplied with a list of templates as an argument, it will return a\n"
	"list of only those tuples that match at least one template.\n"
	"\n"
	"The log() method prints a running log of tuple server activity to\n"
	"stdout. It should be called in a \"while 1:\" loop.";


/**
 * \brief
 */
static PyMethodDef ctx_methods[] = {
	{"put", ctx_put_tuple, METH_VARARGS,
	 ctx_put_doc_string},
	{"get", ctx_get_tuple, METH_VARARGS,
	 ctx_get_doc_string},
	{"read", ctx_read_tuple, METH_VARARGS,
	 ctx_read_doc_string},
	{"get_nonblocking", ctx_get_nb_tuple, METH_VARARGS,
	 ctx_get_nb_doc_string},
	{"read_nonblocking", ctx_read_nb_tuple, METH_VARARGS,
	 ctx_read_nb_doc_string},
	{"replace", ctx_replace_tuple, METH_VARARGS,
	 ctx_replace_doc_string},
	{"dump", ctx_dump_tuple_space, METH_VARARGS,
	 ctx_dump_doc_string},
	{"count", ctx_count_tuple_space, METH_VARARGS,
	 ctx_count_doc_string},
	{"log", ctx_tuple_server_log, METH_VARARGS,
	 ctx_log_doc_string},
	{NULL, NULL, 0, NULL}	/* sentinel */
};

/**
 * \brief
 */
static PyObject *
ctx_getattr(CtxObject * obj, char *name)
{
	if (strcmp(name, "__doc__") == 0) {
		return PyString_FromString(ctx_doc_string);
	}
	return Py_FindMethod(ctx_methods, (PyObject *) obj, name);
}


#define SETENV_STRING \
"\nUnable to find the environment variables (LINUXTUPLES_HOST,\n" \
"LINUXTUPLES_PORT). Please set them."


static char
	new_ctx_doc_string[] =
	"conn = connect()	   [ get host and port# from env. vars]\n"
	"conn = connect(string,int)   [ set host and port# explicitly ]\n"
	"Return a connection to the tuple server.";

/**
 * \brief
 */
static PyObject *
new_ctx(PyObject * self, PyObject * args)
{
	CtxObject *pyctx = NULL;
	char *host = NULL;
	int port = -1;
	if (PyArg_ParseTuple(args, "|si", &host, &port)) {
		pyctx = PyObject_New(CtxObject, &CtxType);
		pyctx->ctx.id = 0;
		if (host != NULL) {
			if (port == -1) {
				PyErr_SetString(PyExc_TypeError,
						"try connect(hostname,"
						" portnumber)");
				return NULL;
			}
			strcpy(pyctx->ctx.peername, host);
			pyctx->ctx.portnumber = port;
		}
		else if (get_server_portnumber(&pyctx->ctx)) {
			PyErr_SetString(PyExc_ImportError, SETENV_STRING);
			return NULL;
		}
	}
	/* If bad args, return NULL */
	return (PyObject *) pyctx;
}

/**
 * \brief
 */
static void
ctx_dealloc(PyObject * self)
{
	CtxObject *pyctx = (CtxObject *) self;
	close(pyctx->ctx.sock);
	PyObject_Del(self);
}

/**
 * \brief
 */
static PyTypeObject CtxType = {
	PyObject_HEAD_INIT(NULL)
		0,
	"Ctx",
	sizeof(CtxObject),
	0,
	ctx_dealloc,		/*tp_dealloc */
	0,			/*tp_print */
	(getattrfunc) ctx_getattr,	/*tp_getattr */
	0,			/*tp_setattr */
	0,			/*tp_compare */
	0,			/*tp_repr */
	0,			/*tp_as_number */
	0,			/*tp_as_sequence */
	0,			/*tp_as_mapping */
	0,			/*tp_hash */
};



/**
 * \brief Pack a Python tuple into a Linuxtuples tuple.
 *
 * If it's for a PUT, do not permit '?' wildcards. But they're OK for
 * GET and READ templates.
 */
static struct tuple *
PyTuple_to_LinuxTuple(PyObject * pytup, int puttable)
{
	int i, len;
	struct tuple *t;
	len = PyTuple_Size(pytup);
	t = malloc(sizeof(struct tuple));
	if (t == NULL) {
		return (struct tuple *) PyErr_NoMemory();
	}
	t->num_elts = len;
	t->elements = malloc(t->num_elts * sizeof(struct element));
	if (t->elements == NULL) {
		return (struct tuple *) PyErr_NoMemory();
	}
	t->string_space = NULL;
	t->string_length = 0;

	for (i = 0; i < len; i++) {
		PyObject *elem;
		elem = PyTuple_GetItem(pytup, i);
		if (elem == Py_None) {
			if (puttable)
				goto bad_element;
			t->elements[i].tag = '?';
		}
		else if (PyInt_Check(elem)) {
			t->elements[i].tag = 'i';
			t->elements[i].data.i = PyInt_AsLong(elem);
		}
		else if (PyFloat_Check(elem)) {
			t->elements[i].tag = 'd';
			t->elements[i].data.d = PyFloat_AsDouble(elem);
		}
		else if (PyString_Check(elem)) {
			int n = PyString_Size(elem);
			t->elements[i].tag = 's';
			t->elements[i].data.s.ptr = PyString_AsString(elem);
			t->elements[i].data.s.len = n;
			t->string_length += n;
		}
		else if (PyTuple_Check(elem)) {
			t->elements[i].tag = 't';
			t->elements[i].data.t = PyTuple_to_LinuxTuple(elem, puttable);
		}
		else {
		      bad_element:
			PyErr_SetString(PyExc_RuntimeError,
					"Bad element in LinuxTuples tuple");
			return NULL;
		}
	}
	return t;
}


/**
 * \brief Unpack a Linuxtuples tuple and convert it to a Python tuple.
 */
static PyObject *
LinuxTuple_to_PyTuple(struct tuple *t)
{
	PyObject *pytup;
	int i;
	/*
	 * Unpack the Linuxtuples tuple to a Python tuple.
	 */
	pytup = PyTuple_New(t->num_elts);
	if (pytup == NULL)
		return PyErr_NoMemory();
	for (i = 0; i < t->num_elts; i++) {
		PyObject *ob;
		switch (t->elements[i].tag) {
		case '?':
			Py_INCREF(Py_None);
			PyTuple_SetItem(pytup, i, Py_None);
			break;
		case 'i':
			ob = PyInt_FromLong(t->elements[i].data.i);
			if (ob == NULL)
				return PyErr_NoMemory();
			PyTuple_SetItem(pytup, i, ob);
			break;
		case 'd':
			ob = PyFloat_FromDouble(t->elements[i].data.d);
			if (ob == NULL)
				return PyErr_NoMemory();
			PyTuple_SetItem(pytup, i, ob);
			break;
		case 's':
			ob = PyString_FromStringAndSize(t->elements[i].data.
							s.ptr,
							t->elements[i].data.
							s.len);
			if (ob == NULL)
				return PyErr_NoMemory();
			PyTuple_SetItem(pytup, i, ob);
			break;
		case 't':
			ob = LinuxTuple_to_PyTuple(t->elements[i].data.t);
			PyTuple_SetItem(pytup, i, ob);
			break;
		}
	}
	return pytup;
}


static char
	ctx_put_doc_string[] =
	"conn.put(tuple)\n" "Put a tuple into tuple space.";


/**
 * \brief
 */
static PyObject *
ctx_put_tuple(PyObject * self, PyObject * args)
{
	CtxObject *pyctx = (CtxObject *) self;
	PyObject *pytup;
	struct tuple *t;

	if (!PyArg_ParseTuple(args, "O", &pytup))
		return NULL;
	if (!PyTuple_Check(pytup)) {
		PyErr_SetString(PyExc_TypeError, "argument must be a tuple");
		return NULL;
	}

	t = PyTuple_to_LinuxTuple(pytup, 1);
	if (t == NULL) {
		PyErr_SetString(PyExc_TypeError,
				"PyTuple_to_LinuxTuple failed "
				"for some reason");
		return NULL;
	}

	/*
	 * Put the Linuxtuples tuple in the tuple space.
	 */

	if (put_tuple(t, &pyctx->ctx)) {
		PyErr_SetString(PyExc_RuntimeError, "put() failed");
		return NULL;
	}
	destroy_tuple(t);

	Py_INCREF(Py_None);
	return Py_None;
}


static char
	ctx_replace_doc_string[] =
	"conn.replace(templ, replacement)\n" "Remove all tuples matching the template, and put a single replacement into the space.";

/**
 * \brief
 */
static PyObject *
ctx_replace_tuple(PyObject *self, PyObject *args)
{
	CtxObject *pyctx = (CtxObject *) self;
        PyObject *pytemplate, *pyreplacement;
        struct tuple *template, *replacement;

        if (!PyArg_ParseTuple(args, "OO", &pytemplate, &pyreplacement))
                return NULL;
        if (!PyTuple_Check(pytemplate) || !PyTuple_Check(pyreplacement)) {
                PyErr_SetString(PyExc_TypeError, "argument must be a tuple");
                return NULL;
        }

        template = PyTuple_to_LinuxTuple(pytemplate, 0); // can be non-putable
        replacement = PyTuple_to_LinuxTuple(pyreplacement, 1); // putable
        if (template == NULL || replacement == NULL) {
                PyErr_SetString(PyExc_TypeError,
                                "PyTuple_to_LinuxTuple failed "
                                "for some reason");
                return NULL;
        }

	/*
	 * Remove tuples matching the template
	 */


        /*
         * Put the Linuxtuples tuple in the tuple space.
         */

        if (replace_tuple(template, replacement, &pyctx->ctx)) {
                PyErr_SetString(PyExc_RuntimeError, "replace() failed");
                return NULL;
        }
        destroy_tuple(template);
        destroy_tuple(replacement);

        Py_INCREF(Py_None);
        return Py_None;
}



/**
 * \brief
 */
static PyObject *
ctx_get_read(CtxObject * pyctx,
	     PyObject * args,
	     struct tuple *(*func) (struct tuple *,
				    struct context *), int blocking)
{
	PyObject *pytup;
	struct tuple *s, *t;

	if (!PyArg_ParseTuple(args, "O", &pytup))
		return NULL;
	if (!PyTuple_Check(pytup)) {
		PyErr_SetString(PyExc_TypeError, "argument must be a tuple");
		return NULL;
	}

	/*
	 * Build a LinuxTuples tuple template.
	 */
	s = PyTuple_to_LinuxTuple(pytup, 0);
	if (s == NULL) {
		return NULL;
	}

	/*
	 * Get a Linuxtuples tuple from the tuple space.
	 */
	Py_BEGIN_ALLOW_THREADS
	t = (*func) (s, &pyctx->ctx);
	Py_END_ALLOW_THREADS
	destroy_tuple(s);
	if (t == (struct tuple *) -1) {
		goto abject_failure;
	}
	if (!blocking && t == NULL) {
		Py_INCREF(Py_None);
		return Py_None;
	}
	if (t == NULL) {
	      abject_failure:
		PyErr_SetString(PyExc_RuntimeError, "get() or read() failed");
		return NULL;
	}

	/*
	 * Unpack the Linuxtuples tuple to a Python tuple.
	 */
	pytup = LinuxTuple_to_PyTuple(t);
	destroy_tuple(t);
	return pytup;
}


static char
	ctx_get_doc_string[] =
	"tuple = conn.get(template)\n"
	"Get a tuple from tuple space. Items in the returned tuple\n"
	"must match any non-None items in the template. Nones are\n"
	"wildcards. This call blocks until a matching tuple is found.";

/**
 * \brief
 * \sa ctx_get_doc_string
 */
static PyObject *
ctx_get_tuple(PyObject * self, PyObject * args)
{
	return ctx_get_read((CtxObject *) self, args, get_tuple, 1);
}

static char
	ctx_read_doc_string[] =
	"tuple = conn.read(template)\n"
	"Read a copy of a tuple from tuple space. Items in the\n"
	"returned tuple must match any non-None items in the template.\n"
	"Nones are wildcards. This call blocks until a matching\n"
	"tuple is found.";

/**
 * \brief
 * \sa ctx_read_doc_string
 */
static PyObject *
ctx_read_tuple(PyObject * self, PyObject * args)
{
	return ctx_get_read((CtxObject *) self, args, read_tuple, 1);
}

static char
	ctx_get_nb_doc_string[] =
	"tuple = conn.get_nonblocking(template)\n"
	"Get a tuple from tuple space. Items in the returned tuple\n"
	"must match any non-None items in the template. Nones are\n"
	"wildcards. If no matching tuple is found, returns None.";

/**
 * \brief
 * \sa ctx_get_nb_doc_string
 */
static PyObject *
ctx_get_nb_tuple(PyObject * self, PyObject * args)
{
	return ctx_get_read((CtxObject *) self, args, get_nb_tuple, 0);
}

static char
	ctx_read_nb_doc_string[] =
	"tuple = conn.read_nonblocking(template)\n"
	"Read a copy of a tuple from tuple space. Items in the\n"
	"returned tuple must match any non-None items in the template.\n"
	"Nones are wildcards. If no matching tuple is found, returns\n"
	"None.";

/**
 * \brief
 * \sa ctx_read_nb_doc_string
 */
static PyObject *
ctx_read_nb_tuple(PyObject * self, PyObject * args)
{
	return ctx_get_read((CtxObject *) self, args, read_nb_tuple, 0);
}

static char
	ctx_dump_doc_string[] =
	"tuple_list = conn.dump( [template_list] )\n"
	"Return a list of the tuples in the tuple space. If no template\n"
	"list is given, return all the tuples. If there is a template\n"
	"list, return only the tuples that match at least one template.";

/**
 * \brief
 * \sa ctx_dump_doc_string
 */
static PyObject *
ctx_dump_tuple_space(PyObject * self, PyObject * args)
{
	PyObject *pylst = NULL, *pytup;
	CtxObject *pyctx;
	struct tuple_list *slist, *tlist, *x;
	int i, count, listsize;

	pyctx = (CtxObject *) self;
	if (!PyArg_ParseTuple(args, "|O", &pylst))
		return NULL;
	if (pylst != NULL) {
		if (!PyList_Check(pylst)) {
			PyErr_SetString(PyExc_TypeError,
					"argument must be a list");
			return NULL;
		}
		listsize = PyList_Size(pylst);
	}
	else {
		listsize = 0;
	}


	/* Assemble a list of templates from the arg.
	 */
	slist = NULL;
	for (i = 0; i < listsize; i++) {
		pytup = PyList_GetItem(pylst, i);
		if (!PyTuple_Check(pytup)) {
			PyErr_SetString(PyExc_TypeError,
					"list members must be tuples");
			return NULL;
		}
		x = malloc(sizeof(struct tuple_list));
		if (x == NULL)
			return PyErr_NoMemory();
		x->tup = PyTuple_to_LinuxTuple(pytup, 0);
		x->next = slist;
		slist = x;
	}

	x = slist;
	while (x != NULL) {
		x = x->next;
	}

	/* Do a dump request to the server.
	 */
	tlist = dump_tuple_space(slist, &pyctx->ctx);
	while (slist) {
		x = slist;
		slist = slist->next;
		free(x);
	}

	count = 0;
	x = tlist;
	while (x != NULL) {
		count++;
		x = x->next;
	}
	pylst = PyList_New(count);
	if (pylst == NULL)
		return PyErr_NoMemory();
	for (i = 0, x = tlist; x != NULL; x = x->next, i++)
		PyList_SetItem(pylst, i, LinuxTuple_to_PyTuple(x->tup));

	while (tlist) {
		x = tlist;
		tlist = tlist->next;
		free(x);
	}

	return pylst;
}

static char
	ctx_count_doc_string[] =
	"n = conn.count( [template_list] )\n"
	"Return a count of the tuples in the tuple space. If no template\n"
	"list is given, count all the tuples. If there is a template\n"
	"list, return only the number of tuples that match at least one\n"
	"template.";

/**
 * \brief
 * \sa ctx_dump_doc_string
 */
static PyObject *
ctx_count_tuple_space(PyObject * self, PyObject * args)
{
	PyObject *pylst = NULL, *pytup;
	CtxObject *pyctx;
	struct tuple_list *slist, *x;
	int i, count, listsize;

	pyctx = (CtxObject *) self;
	if (!PyArg_ParseTuple(args, "|O", &pylst))
		return NULL;
	if (pylst != NULL) {
		if (!PyList_Check(pylst)) {
			PyErr_SetString(PyExc_TypeError,
					"argument must be a list");
			return NULL;
		}
		listsize = PyList_Size(pylst);
	}
	else {
		listsize = 0;
	}


	/* Assemble a list of templates from the arg.
	 */
	slist = NULL;
	for (i = 0; i < listsize; i++) {
		pytup = PyList_GetItem(pylst, i);
		if (!PyTuple_Check(pytup)) {
			PyErr_SetString(PyExc_TypeError,
					"list members must be tuples");
			return NULL;
		}
		x = malloc(sizeof(struct tuple_list));
		if (x == NULL)
			return PyErr_NoMemory();
		x->tup = PyTuple_to_LinuxTuple(pytup, 0);
		x->next = slist;
		slist = x;
	}

	x = slist;
	while (x != NULL) {
		x = x->next;
	}

	/* Do a count request to the server.
	 */
	i = count_tuple_space(slist, &pyctx->ctx, &count);

	while (slist) {
		x = slist;
		slist = slist->next;
		free(x);
	}

	if (i != 0)
		return NULL;

	return PyInt_FromLong(count);
}

static char
	ctx_log_doc_string[] =
	"Dump a continuous log of tuple space activity to stdout. Call this\n"
	"method in a \"while 1:\" loop. Looping in Python rather than C\n"
	"means that you can break out of the loop easily with control-C,\n"
	"and you can put other code in the loop.";

/**
 * \brief
 * \sa ctx_log_doc_string
 */
static PyObject *
ctx_tuple_server_log(PyObject * self, PyObject * args)
{
	CtxObject *pyctx = (CtxObject *) self;
	if (!PyArg_ParseTuple(args, ""))
		return NULL;
	tuple_server_log(stdout, &pyctx->ctx);
	Py_INCREF(Py_None);
	return Py_None;
}

static char
	py_random_int_doc_string[] =
	"Return a 32-bit random integer, taken from /dev/urandom.";

/**
 * \brief
 * \sa py_random_int_doc_string
 */
static PyObject *
py_random_int(PyObject * self, PyObject * args)
{
	if (!PyArg_ParseTuple(args, ""))
		return NULL;
	return PyInt_FromLong(random_int());
}



/**
 * \brief List of functions defined in the linuxtuples module.
 */
static PyMethodDef linuxtuples_methods[] = {
	{"connect", new_ctx, METH_VARARGS, new_ctx_doc_string},
	{"random", py_random_int, METH_VARARGS, py_random_int_doc_string},
	{NULL, NULL}		/* sentinel */
};


/**
 * \brief Initialization function for the module _must_ be called
 * initlinuxtuples.
 */
DL_EXPORT(void)
initlinuxtuples()
{
	PyObject *m, *d;

	CtxType.ob_type = &PyType_Type;
	m = Py_InitModule("linuxtuples", linuxtuples_methods);
	d = PyModule_GetDict(m);
	PyDict_SetItemString(d, "__doc__",
			     PyString_FromString(linuxtuples_doc_string));
}
