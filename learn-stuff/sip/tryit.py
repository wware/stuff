#!/usr/bin/env python

import os

if not os.path.exists("word.so"):
    if not os.path.exists("word.o"):
        assert os.system("g++ -c -o word.o word.cpp") == 0
    import sipconfig
    build_file = "word.sbf"
    config = sipconfig.Configuration()
    assert os.system(" ".join([config.sip_bin, "-c", ".", "-b", build_file, "word.sip"])) == 0
    makefile = sipconfig.SIPModuleMakefile(config, build_file)
    makefile.extra_lflags.append("word.o")
    makefile.generate()
    os.system("make word.so")

import word

w = word.Word("abcde")
print w
print w.content()
