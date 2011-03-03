Serial port stuff for the SAM7
==============================

I need a simple command interpreter that runs via the serial port. Like Forth,
it should be flexible possibly to the point of being Turing-complete. Let's
remember what defines Forth.

- Data and return stacks.
- Pseudocode (or sequences of subroutine calls).
- The dictionary that defines named things.
- Trivial syntax, symbols are parsed in the order presented, with minimal
  state bits.

Let's define this language in terms of a bytecode, and figure out later what
the source language should look like.

Bytecodes should assume reasonable types. Numerical types can be byte, int
(four bytes) or float (00, 01, 10). If 11 appears in the numerical type field
then either the bytecode is unrelated to typed data, or the type is funky in
some way. Pointers of any sort are

Operations for bytecodes include addition, subtraction, negation,
multiplication, division, modulo, logical AND, logical OR, logical NOT,
bitwise AND, bitwise OR, bitwise NOT. This is looking like an incrementally
compiled source language with C-like syntax. So there needs to be a lex&yacc
parser running on the SAM7.

::

 sudo apt-get install flex bison




..
   Local variables:
   mode: rst
   End:
