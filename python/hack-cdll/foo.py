# https://docs.python.org/2/library/ctypes.html

import os
from ctypes import *

c_code = """\
int add(int x, int y)
{
	return x + y;
}
"""

open("foo.c", "w").write(c_code)
os.system("gcc -dynamiclib -o libfoo.dylib foo.c")

name = "./libfoo.dylib"
lib = cdll.LoadLibrary(name) 

print lib.add(3, 5)
