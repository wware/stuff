Embedding Python in a C program
===============================

Given a legacy C program it may be desirable to maintain the main() function
in the legacy code, and let it call out to a Python interpreter. The C code
can define Python functions and variables to allow Python to manipulate things
in the C world. In this manner one adds scripting to a legacy C program.

This is almost certainly not the approach you'd take with a new program. The
preferable approach would be to write everything in Python initially and
translate pieces of it to C only where necessary for reasons of performance,
or security, or access to resources not directly available in Python. So the
decision about which pieces to translate might be made with the help of a
profiler to identify the slow pieces, and a reference implementation might
be maintained in Python for purposes of regression testing.

Similar considerations would apply to other higher-level-than-C languages such
as Lisp, Scheme, or Java.
