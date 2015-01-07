#!/usr/bin/env python
"""
The magic of Python's ctypes library
https://docs.python.org/2/library/ctypes.html

For *real* fun, type:
  $ gdb `which python`
  ...
  (gdb) break foo.c:3
  (gdb) run foo.py
  Breakpoint 1, add (x=3, y=5) at foo.c:3
  3	    return x + y;
  (gdb) p x
  $1 = 3
  (gdb) p y
  $2 = 5
"""

# Create a C file with a function to be called.
open("foo.c", "w").write("""\
int add(int x, int y)
{
    return x + y;
}
""")

# Compile the C code into a dynamic library.
import os
if os.popen("uname -s").read().strip() == "Darwin":
    # OSX
    os.system("gcc -g -dynamiclib -o libfoo.dylib foo.c")
    name = "./libfoo.dylib"
else:
    # Linux
    os.system("gcc -g -shared -o libfoo.so foo.c")
    name = "./libfoo.so"

# Import the library using ctypes.cdll and exercise the function.
import ctypes
lib = ctypes.cdll.LoadLibrary(name)
assert lib.add(3, 5) == 8

# Bad argument type should fail
try:
    lib.add(3.14, 6.28)
    raise Exception("non-integer argument should be rejected")
except ctypes.ArgumentError:
    pass
