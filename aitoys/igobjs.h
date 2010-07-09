#define ROMSIZE 40000
#define NUMCONSTS 200

#define NUMVARS 100
#define STACKDEPTH 100

typedef struct sProgram {
  unsigned int data[ROMSIZE];
  unsigned int compileptr;
  unsigned int startaddr;
} Program;

typedef struct sThread {
  Program *myProgram;
  int data_stack[STACKDEPTH];
  int variables[NUMVARS];
  unsigned int return_stack[STACKDEPTH];
  unsigned int dsptr, rsptr, stopped, instrucptr;
} Thread;
