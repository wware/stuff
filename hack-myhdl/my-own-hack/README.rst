Parser for C-like syntax
========================

I want to do MyHDL-style hacks with microcontroller code. That is, I want to
write code in Python (or a subset) and then translate the parse trees to code
that can run on an ARM processor.

I'm not really thinking a LALR parser, so the name of the directory should
change. I'm really thinking of doing what the MyHDL guy did, of somehow
converting a Python parse tree to C code that's buildable with an ARM
cross-compiler.

Since Python is a stack-based language, it's likely this will mean emitting
Forth-like pseudocode and then linking it to a Forth-esque interpreter. It
probably makes sense to just take an existing Forth-for-ARM and strap this
thing onto the side.
