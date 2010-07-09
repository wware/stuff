#!/usr/bin/python

import sys, string

opcounter = -1

class Operation:
    def __init__(self, lines):
        global opcounter
        self.count = opcounter
        opcounter = opcounter - 1
        while 1:
            description = string.split(lines.pop(0))
            if description[0] != '#':
                break
        self.name = description.pop(0)
        flag = description.pop(0)
        if flag not in ['D', 'DI', 'R']:
            description = [flag] + description
            flag = 'I'
        self.flag = flag
        if flag != 'R':
            n = description.index('->')
            self.in_args = description[:n]
            self.in_args.reverse()
            self.out_args = description[n+1:]
        else:
            self.in_args = self.out_args = [ ]
        clines = [ ]
        while 1:
            nextline = lines.pop(0)
            if nextline[0] == '#':
                pass
            elif nextline[0] == '%':
                break
            else:
                clines.append(nextline)
        self.clines = clines
    def enumtype(self, typename, func):
        if typename == 'int':
            count = self.count
            name = self.name
        elif typename == 'raw':
            count = self.count - 200
            name = self.name
        else:
            count = self.count - 400
            name = "D" + self.name
        apply (func, (name, count))
    def ctype(self, typename):
        varlist = [ ]
        in_args, out_args = self.in_args, self.out_args
        for x in in_args + out_args:
            if x not in varlist:
                varlist.append(x)
        if typename == 'int':
            push, pop = 'Thread_push', 'Thread_pop'
            name = self.name
        elif typename == 'raw':
            name = self.name
            varlist = in_args = out_args = [ ]
        else:
            push, pop = 'Thread_double_push', 'Thread_double_pop'
            name = "D" + self.name
        indent = "      "
        str = "case " + name + ":\n    {\n"
        if varlist:
            str = str + "      " + typename + " "
            for i in range(len(varlist) - 1):
                str = str + varlist[i] + ", "
            str = str + varlist[-1] + ";\n"
        for x in in_args:
            str = str + indent + x + " = " + pop + " (self);\n"
        if typename != 'raw':
            str = str + indent + 'CURIOUS(printf("%s\\n"));\n'  % name
        for x in self.clines:
            str = str + indent + x
        for x in out_args:
            str = str + indent + push + " (self, " + x + ");\n"
        return str + "    }\n    break;"
    def c(self):
        if self.flag == 'I':
            print self.ctype('int')
        elif self.flag == 'D':
            print self.ctype('double')
        elif self.flag == 'R':
            print self.ctype('raw')
        else:
            print self.ctype('int')
            print self.ctype('double')
    def enum(self, func):
        if self.flag == 'I':
            self.enumtype('int', func)
        elif self.flag == 'D':
            self.enumtype('double', func)
        elif self.flag == 'R':
            self.enumtype('raw', func)
        else:
            self.enumtype('int', func)
            self.enumtype('double', func)

L = sys.stdin.readlines()
ops = [ ]
while L:
    try:
        op = Operation(L)
        ops.append(op)
    except IndexError:
        pass

if sys.argv[1] == "-c":
    for op in ops:
        op.c()
elif sys.argv[1] == "-d":
    def enumfunc(name,count):
        print '#define %s %d' % (name, count)
    for op in ops:
        op.enum(enumfunc)
elif sys.argv[1] == "-p":
    print 'forward = {'
    def enumfunc(name,count):
        print '  "%s": %d,' % (name, count)
    for op in ops:
        op.enum(enumfunc)
    print '}'
    print 'backward = {'
    def enumfunc(name,count):
        print '  %d: "%s",' % (count, name)
    for op in ops:
        op.enum(enumfunc)
    print '}'
