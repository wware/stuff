#include <stdio.h>

#include "Python.h"

int main(void)
{
    printf("Hello, world\n");

    // run a few lines of Python
    Py_Initialize();
    PyRun_SimpleString("import foobar\n"
		       "foo = foobar.Foo()\n"
		       "foo.bar()\n");
    Py_Finalize();
    return 0;
}
