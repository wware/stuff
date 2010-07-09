/* Directed graph object for Python
 */

#include "Python.h"
#include "common.h"
#include "graphobject.h"

staticforward PyTypeObject Graphtype;
staticforward PyTypeObject Ruletype;

#define is_graphobject(v)		((v)->ob_type == &Graphtype)
#define is_ruleobject(v)		((v)->ob_type == &Ruletype)

static PyObject *
newgraphobject(PyObject *self, PyObject *args)
{
	graphobject *xp;
	long size;
	if (!PyArg_ParseTuple(args, "l", &size))
		return NULL;
	xp = PyObject_NEW(graphobject, &Graphtype);
	if (xp == NULL)
		return NULL;
	xp->pointer = 0;
	xp->size = size;
	xp->triples = malloc(size * sizeof(struct triple));
	if (xp->triples == NULL) {
		PyObject_DEL(xp);
		return NULL;
	}
	return (PyObject *) xp;
}

/* Graph methods */

static PyObject *
graph_add(graphobject *self, PyObject *args) {
	long subj, pred, obj;
	if (!PyArg_ParseTuple(args, "lll", &subj, &pred, &obj))
		return NULL;
	add(self->triples, &(self->pointer), self->size,
	    subj, pred, obj);
	if (out_of_memory_flag)
		return PyErr_NoMemory();
	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
graph_size(graphobject *self, PyObject *args) {
	if (!PyArg_ParseTuple(args, ""))
		return NULL;
	return PyInt_FromLong((long)self->pointer);
}

static PyObject *
graph_dump(graphobject *self, PyObject *args) {
	if (!PyArg_ParseTuple(args, ""))
		return NULL;
	PyObject *lst = PyList_New(self->pointer);
	int i;
	for (i = 0; i < self->pointer; i++) {
		struct triple *tr = &(self->triples[i]);
		PyObject *tpl = PyTuple_New(3);
		PyTuple_SetItem(tpl, 0, PyInt_FromLong(tr->subj));
		PyTuple_SetItem(tpl, 1, PyInt_FromLong(tr->pred));
		PyTuple_SetItem(tpl, 2, PyInt_FromLong(tr->obj));
		PyList_SetItem(lst, i, tpl);
	}
	return lst;
}

static PyObject *
tripleToPyTuple(struct triple *t) {
	PyObject *p = PyTuple_New(3);
	PyTuple_SetItem(p, 0, PyInt_FromLong(t->subj));
	PyTuple_SetItem(p, 1, PyInt_FromLong(t->pred));
	PyTuple_SetItem(p, 2, PyInt_FromLong(t->obj));
	return p;
}

static PyObject *
graph_query(graphobject *self, PyObject *args) {
	PyObject *tpl;
	struct triple tr;
	struct substitution *fake = NULL;
	int i;
	if (!PyArg_ParseTuple(args, "O", &tpl))
		return NULL;
	if (!get_triple(tpl, &tr.subj, &tr.pred, &tr.obj)) {
		PyErr_SetString(PyExc_TypeError,
				"argument must be a 3-tuple");
		return NULL;
	}
	PyObject *lst = PyList_New(0);
	for (i = 0; i < self->pointer; i++) {
		struct triple *t = &self->triples[i];
		if (triple_matches(&tr, t, &fake)) {
			PyList_Append(lst,
				      tripleToPyTuple(t));
		}
	}
	return lst;
}

static PyObject *
graph_apply(graphobject *self, PyObject *args)
{
	PyObject *ruleobj;
	if (!PyArg_ParseTuple(args, "O", &ruleobj))
		return NULL;
	if (!is_ruleobject(ruleobj)) {
		PyErr_SetString(PyExc_TypeError,
				"argument must be a rule");
	}
	apply_rule(self->triples,
		   &(self->pointer),
		   self->size,
		   ((ruleobject *)ruleobj)->rule);
	Py_INCREF(Py_None);
	return Py_None;
	return NULL;
}

static void
graph_dealloc(graphobject *xp)
{
	free(xp->triples);
	PyObject_DEL(xp);
}

static PyMethodDef
graphobj_methods[] = {
	{"add", (PyCFunction)graph_add, METH_VARARGS},
	{"size", (PyCFunction)graph_size, METH_VARARGS},
	{"query", (PyCFunction)graph_query, METH_VARARGS},
	{"toList", (PyCFunction)graph_dump, METH_VARARGS},
	{"apply", (PyCFunction)graph_apply, METH_VARARGS},
	{NULL, NULL}
};

static PyObject *
graph_getattr(graphobject *xp, char *name)
{
	return Py_FindMethod(graphobj_methods, (PyObject *)xp, name);
}

static PyTypeObject
Graphtype = {
	PyObject_HEAD_INIT(&PyType_Type)
	0,			/*ob_size*/
	"graph",			/*tp_name*/
	sizeof(graphobject),	/*tp_basicsize*/
	0,			/*tp_itemsize*/
	/* methods */
	(destructor)graph_dealloc, /*tp_dealloc*/
	0,			/*tp_print*/
	(getattrfunc)graph_getattr, /*tp_getattr*/
	0,			/*tp_setattr*/
	0,			/*tp_compare*/
	0,			/*tp_repr*/
	0,			/*tp_as_number*/
	0,			/*tp_as_sequence*/
	0,			/*tp_as_mapping*/
	0,			/*tp_hash*/
};

/* ------------------------------------------ */

static int
get_triple(PyObject *item,
	   int *s, int *p, int *o) {
	PyObject *subitem;
	if (!PyTuple_Check(item)) return 0;
	if (PyTuple_Size(item) != 3) return 0;

	subitem = PyTuple_GetItem(item, 0);
	if (PyInt_Check(subitem))
		*s = (int) PyInt_AsLong(subitem);
	else return 0;

	subitem = PyTuple_GetItem(item, 1);
	if (PyInt_Check(subitem))
		*p = (int) PyInt_AsLong(subitem);
	else return 0;

	subitem = PyTuple_GetItem(item, 2);
	if (PyInt_Check(subitem))
		*o = (int) PyInt_AsLong(subitem);
	else return 0;

	return 1;
}

static int
get_triple_from_many(PyObject *tpl, int index,
		     int *s, int *p, int *o) {
	return get_triple(PyTuple_GetItem(tpl, index), s, p, o);
}

static PyObject *
newruleobject(PyObject *self, PyObject *args)
{
	ruleobject *xp;
	xp = PyObject_NEW(ruleobject, &Ruletype);
	if (xp == NULL)
		return NULL;
	int i, size;
	size = PyTuple_Size(args);
	int s, p, o;
	if (!get_triple_from_many(args, size - 1, &s, &p, &o)) {
		PyErr_SetString(PyExc_TypeError,
				"bad conclusion");
		return NULL;
	}
	xp->rule = make_rule(NULL, s, p, o);
	for (i = 0; i < size - 1; i++) {
		if (!get_triple_from_many(args, i, &s, &p, &o)) {
			PyErr_SetString(PyExc_TypeError,
					"bad premise");
			return NULL;
		}
		rule_add_premise(xp->rule, s, p, o);
	}
	return (PyObject *) xp;
}

/* Rule methods */

static void
rule_dealloc(ruleobject *xp)
{
	free_rules(xp->rule);
	PyObject_DEL(xp);
}

static PyMethodDef
ruleobj_methods[] = {
	{NULL, NULL}
};

static PyObject *
rule_getattr(ruleobject *xp, char *name)
{
	return Py_FindMethod(ruleobj_methods, (PyObject *)xp, name);
}

static PyTypeObject
Ruletype = {
	PyObject_HEAD_INIT(&PyType_Type)
	0,			/*ob_size*/
	"rule",			/*tp_name*/
	sizeof(ruleobject),	/*tp_basicsize*/
	0,			/*tp_itemsize*/
	/* methods */
	(destructor)rule_dealloc, /*tp_dealloc*/
	0,			/*tp_print*/
	(getattrfunc)rule_getattr, /*tp_getattr*/
	0,			/*tp_setattr*/
	0,			/*tp_compare*/
	0,			/*tp_repr*/
	0,			/*tp_as_number*/
	0,			/*tp_as_sequence*/
	0,			/*tp_as_mapping*/
	0,			/*tp_hash*/
};

/* ------------------------------------------ */

static PyObject *
graph_demo(graphobject *self, PyObject *args)
{
	if (!PyArg_ParseTuple(args, ":demo"))
		return NULL;
	printf("Here is the demo function, which does nothing\n");
	Py_INCREF(Py_None);
	return Py_None;
}

static PyMethodDef
graph_methods[] = {
	{"rule", (PyCFunction)newruleobject, METH_VARARGS},
	{"graph", (PyCFunction)newgraphobject, METH_VARARGS},
	{"demo", (PyCFunction)graph_demo, METH_VARARGS},
	{NULL, NULL}
};

void
initgraph(void)
{
	PyImport_AddModule("graph");
	Py_InitModule("graph", graph_methods);
}

/*
 * Local Variables:
 * c-basic-offset: 8
 * tab-width: 8
 * End:
 */
