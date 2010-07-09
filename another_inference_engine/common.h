// #define DEBUG(x)   x
#define DEBUG(x)

#define WILDCARD 0
#define LOOKUP_FAILED -1

#define VARBIT    0x80000000
#define ISVAR(x)  (((x) & VARBIT) != 0)

struct triple {
	int subj;
	int pred;
	int obj;
};

struct rule {
	struct rule *next;
	struct triple conclusion;
	int numpremises;
	struct triple *premises;
};

struct substitution {
	struct substitution *next;
	int variable;
	int value;
};

#ifdef Py_PYTHON_H
typedef struct {
	PyObject_HEAD
	int pointer;
	int size;
	struct triple *triples;
} graphobject;

typedef struct {
	PyObject_HEAD
	struct rule *rule;
} ruleobject;
#endif

extern int out_of_memory_flag;
