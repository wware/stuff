#ifdef DEBUG
#define _DBG_(x) x
#else
#define _DBG_(x)
#endif

#define DBGPRINTF(format) \
  _DBG_(indent_func(); \
        fprintf(stderr,__FILE__ ":%d " \
                format,__LINE__))
#define DBGPRINTF1(format,a) \
  _DBG_(indent_func(); \
        fprintf(stderr,__FILE__ ":%d " \
                format,__LINE__,a))
#define DBGPRINTF2(format,a,b) \
  _DBG_(indent_func(); \
        fprintf(stderr,__FILE__ ":%d " \
                format,__LINE__,a,b))
#define DBGPRINTF3(format,a,b,c) \
  _DBG_(indent_func(); \
        fprintf(stderr,__FILE__ ":%d " \
                format,__LINE__,a,b,c))
#define DBGPRINTF4(format,a,b,c,d) \
  _DBG_(indent_func(); \
        fprintf(stderr,__FILE__ ":%d " \
                format,__LINE__,a,b,c,d))
#define DBGPRINTF5(format,a,b,c,d,e) \
  _DBG_(indent_func(); \
        fprintf(stderr,__FILE__ ":%d " \
                format,__LINE__,a,b,c,d,e))
#define DBGPRINTF6(format,a,b,c,d,e,f) \
  _DBG_(indent_func(); \
        fprintf(stderr,__FILE__ ":%d " \
                format,__LINE__,a,b,c,d,e,f))
#define DBGPRINTF7(format,a,b,c,d,e,f,g) \
  _DBG_(indent_func(); \
        fprintf(stderr,__FILE__ ":%d " \
                format,__LINE__,a,b,c,d,e,f,g))
#define DBGPRINTF8(format,a,b,c,d,e,f,g,h) \
  _DBG_(indent_func(); \
        fprintf(stderr,__FILE__ ":%d " \
                format,__LINE__,a,b,c,d,e,f,g,h))
#define DBGPRINTF9(format,a,b,c,d,e,f,g,h,i) \
  _DBG_(indent_func(); \
	fprintf(stderr,__FILE__ ":%d " \
                format,__LINE__,a,b,c,d,e,f,g,h,i))
#define MARK()  DBGPRINTF("MARK\n")

/* Asserts stay in effect even when DEBUG is not defined */
#define ASSERT(c) \
    { if (!(c)) { \
    fprintf(stderr, __FILE__ ":%d in %s: ", __LINE__, __FUNCTION__); \
    fprintf(stderr, "assertion failed:\n%s\n\n", #c); exit(1); } }

void indent_func(void);
void enter_thingy(char *filename, int linenum, char *funcname);
void leave_thingy(char *filename, int linenum, char *funcname);

#define ENTER()    \
  _DBG_(enter_thingy(__FILE__,__LINE__,__FUNCTION__))
#define LEAVE()    \
  _DBG_(leave_thingy(__FILE__,__LINE__,__FUNCTION__))


#define DBG_INT(x) \
   DBGPRINTF1(#x " = %d\n", ((int) (x)))
#define DBG_HEX(x) \
   DBGPRINTF1(#x " = 0x%08X\n", ((int) (x)))
#define DBG_FLOAT(x) \
   DBGPRINTF1(#x " = %f\n", ((float) (x)))


#define NEW_THREAD(func) \
  new_thread(func, sizeof(struct func##_frame))


struct thread {
	struct generic_frame *frame;
	struct thread *prev, *next;
};

extern struct thread *current_thread;

struct generic_frame {
	/* all frames should start with these same fields in the
	 * exact same order
	 */
	int (*func) (struct thread *);
	int tag, framesize;
	struct generic_frame *caller;
	struct thread *context;
	void *return_value_pointer;
};

struct pfork_frame {
	int (*func) (struct thread *);
	int tag, framesize;
	struct generic_frame *caller;
	struct thread *context;
	void *return_value_pointer;
	struct thread *new;
};

struct pexit_frame {
	int (*func) (struct thread *);
	int tag, framesize;
	struct generic_frame *caller;
	struct thread *context;
	void *return_value_pointer;
};

void register_symbol(char *name, void *p);
struct generic_frame *new_frame(int (*func) (struct thread *),
				int framesize, struct thread *context);
struct thread *new_thread(int (*func) (struct thread *), int framesize);
int step_thread(int n);
int step_thread_noisy(int n);
int pfork(struct thread *context);
int pexit(struct thread *context);
