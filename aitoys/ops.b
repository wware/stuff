# The format for these things works like this. The first line is a
# description, with the name of the command, a flag telling whether
# the command works with ints or doubles or both, and the stack
# effect. The remaining lines are C code for the command, ending at a
# line starting with a percent sign. The flag works like this: 'DI' is
# ints-and-doubles, 'D' is doubles-only, default is ints-only. In cases
# where it's ints-and-doubles, the command name for the doubles version
# is formed by prepending 'D' to the name, e.g. ADD -> DADD.
# Blank lines are permissible in this file, as are comment lines with
# '#' as the first character.

ADD DI x y -> z
z = x + y;
%
SUB DI x y -> z
z = x - y;
%
MUL DI x y -> z
z = x * y;
%
DIV DI x y -> z
z = x / y;
%
POW D x y -> z
z = pow(x, y);
%
EXP D x -> y
y = exp(x);
%
LOG D x -> y
y = log(x);
%
LOG10 D x -> y
y = log10(x);
%
SIN D x -> y
y = sin(x);
%
COS D x -> y
y = cos(x);
%
TAN D x -> y
y = tan(x);
%
ATAN2 D x y -> z
z = atan2(x, y);
%
ASIN D x -> y
y = asin(x);
%
ACOS D x -> y
y = acos(x);
%
SINH D x -> y
y = sinh(x);
%
COSH D x -> y
y = cosh(x);
%
TANH D x -> y
y = tanh(x);
%
NOT x -> y
y = !x;
%
AND x y -> z
z = x && y;
%
OR x y -> z
z = x || y;
%
LESS x y -> z
z = (x < y);
%
GREATER x y -> z
z = (x > y);
%
EQUAL x y -> z
z = (x == y);
%
UNEQUAL x y -> z
z = (x != y);
%
DEQUAL R
double x, y;
int z;
CURIOUS(printf("DEQUAL\n"));
y = Thread_double_pop (self);
x = Thread_double_pop (self);
z = (x == y);
Thread_push (self, z);
%
DUNEQUAL R
double x, y;
int z;
CURIOUS(printf("DUNEQUAL\n"));
y = Thread_double_pop (self);
x = Thread_double_pop (self);
z = (x != y);
Thread_push (self, z);
%
DLESS R
double x, y;
int z;
CURIOUS(printf("DLESS\n"));
y = Thread_double_pop (self);
x = Thread_double_pop (self);
z = (x < y);
Thread_push (self, z);
%
DGREATER R
double x, y;
int z;
CURIOUS(printf("DGREATER\n"));
y = Thread_double_pop (self);
x = Thread_double_pop (self);
z = (x > y);
Thread_push (self, z);
%
DUP DI x -> x x
%
SWAP DI x y -> y x
%
ROT DI x y z -> y z x
%
DROP DI x ->
%
OVER DI x y -> x y x
%
PICK x -> y
if (self->dsptr - x < 0)
  {
    throw_exception (ERR_INDEX, "Thread pop underflow");
    return;
  }
y = self->data_stack[self->dsptr - n];
%
PUSHCONST -> x
if (self->instrucptr >= ROMSIZE - 1)
  {
    throw_exception (ERR_INDEX, "Badly compiled constant (1)");
    return;
  }
x = prog[self->instrucptr++];
%
PUSHCONST D -> x
if (self->instrucptr >= ROMSIZE - 1)
  {
    throw_exception (ERR_INDEX, "Badly compiled constant (1)");
    return;
  }
x = *((double*) &(prog[self->instrucptr]));
self->instrucptr += 2;
%
JUMP ->
self->instrucptr = (unsigned int) prog[self->instrucptr];
%
JUMPZ x ->
if (!((int) x))
  self->instrucptr = (unsigned int) prog[self->instrucptr];
else
  self->instrucptr++;
%
PRINT x ->
printf("%d ", x);
%
PRINT D x ->
printf("%.12f ", x);
%
RETURN ->
if (self->rsptr == 0)
  {
    /* throw_exception (ERR_INDEX, "Thread return pop underflow"); */
    self->stopped = 1;
    return;
  }
self->instrucptr = self->return_stack[--self->rsptr];
%
STOP ->
self->stopped = 1;
%
CR ->
printf ("\n");
%
