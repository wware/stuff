%module igobjs

%include "igobjs.h"
%include "ops.h"

%{
#include "igobjs.h"
#include "ops.h"

extern PyObject * python_croak (void);
extern void clear_exception (void);
extern void throw_exception (enum error_type et, char *msg);
extern int check_exception (void);
extern Program * new_Program (void);
extern double Program_read_const (Program * self, unsigned int n);
extern void Program_compile (Program * self, int x);
extern void Program_compile_double (Program * self, double x);
extern unsigned int Program___getitem__ (Program * self, unsigned int n);
extern void Program___setitem__ (Program * self, int n, unsigned int x);
extern Thread * new_Thread (Program * myprog);
extern void delete_Thread (Thread * self);
extern double Thread___getitem__ (Thread * self, unsigned int n);
extern void Thread___setitem__ (Thread * self, int n, double x);
extern double Thread_fetchvar (Thread * self, unsigned int n);
extern void Thread_storevar (Thread * self, int n, double x);
extern int Thread_pop (Thread * self);
extern void Thread_push (Thread * self, int x);
extern double Thread_double_pop (Thread * self);
extern void Thread_double_push (Thread * self, double x);
extern void Thread_stack_dump (Thread *self);
extern void Thread_steps (Thread * self, int n);
extern void Thread_step (Thread * self);
extern void Thread_run (Thread * self);
%}

%addmethods Program {
	Program();
	/* char *__repr();  -- use Program___repr__() in C code */
	void compile(int);
	void compile_double(double);
	double read_const (unsigned int);
	unsigned int __getitem__(int);
	void __setitem__(int,unsigned int);
}

%addmethods Thread {
	Thread(Program*);
	~Thread();
	double __getitem__(int);
	void __setitem__(int,double);
	void push(double);
	double pop(void);
	void run(void);
	void steps(int);
	void step(void);
	void storevar(int, double);
	double fetchvar(int);
}



%except(python) {
	clear_exception();
	$function
	if (check_exception()) {
		return python_croak();
	}
}
