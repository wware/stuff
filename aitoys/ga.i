%module ga

%include "ga.h"

%typemap (python,in) PyObject * {
	$target = $source;
}

%typemap (python,out) PyObject * {
	$target = $source;
}

%{
#include "ga.h"

extern PyObject * python_croak (void);
extern void clear_exception (void);
extern void throw_exception (enum error_type et, char *msg);
extern int check_exception (void);
extern Chromosome * new_Chromosome (int size, int maxval, int randomize);
extern void delete_Chromosome (Chromosome * self);
extern double Chromosome___getitem__ (Chromosome * self, unsigned int n);
extern void Chromosome___setitem__ (Chromosome * self, unsigned int n, unsigned int x);
extern Chromosome * Chromosome_cross (Chromosome * self, Chromosome * other);
extern void Chromosome_mutate (Chromosome * self, double probability);
extern PyObject * Chromosome_list(Chromosome *self);
extern PyObject * Chromosome_tuple(Chromosome *self);
extern void sort_by_fitness (PyObject *lst, PyObject *fitness_function);
%}

%extend Chromosome {
	Chromosome(int, int, int);
	~Chromosome();
	unsigned int __getitem__(unsigned int);
	void __setitem__(unsigned int, unsigned int);
	Chromosome *cross(Chromosome *);
	void mutate(double);
	PyObject *list(void);
	PyObject *tuple(void);
}

extern void sort_by_fitness (PyObject *lst, PyObject *fitness_function);

%except(python) {
	clear_exception();
	$function
	if (check_exception()) {
		return python_croak();
	}
}
