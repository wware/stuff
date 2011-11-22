Python Abstract Syntax Tree Hacks
=================================

The best existing example of an AST hack is MyHDL. The idea is that you write
code in Python, where unit testing is available, and then you can translate
the code into some other language or form. MyHDL translates it into Verilog.

Other possible applications would include:

* creating a performant C extension.
* CUDA-friendly C code to run stuff on GPUs.
* Objective C for creating Mac or iPhone apps.
* Java for Android apps or other Java uses.

How I envision this going is that you start by enumerating a bunch of standard
Python operations, and you need to implement those in the target environment.
Then you have some special cases, like how to handle function definitions and
global variables, and you have to figure out how the target environment will
want to deal with those.

The translation to the target environment is likely to be an extension of the
ast.NodeVisitor class, so it would look roughly like a SAX parser.
