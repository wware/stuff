#!/usr/bin/python

"""
Massively multi-threaded C programs

This is a simple way to get thousands of threads in a C program,
where the threads live within a single OS process.

"""

############################################################


import sys, os, string, re


contextSwitchIndex = 0

def trimWhiteSpace(str,
                   begin = re.compile("^[ \t]+"),
                   end = re.compile("[ \t]+$")):
    try:
        a, b = begin.search(str).regs[0]
        str = str[b:]
    except AttributeError:
        pass 
    try:
        c, d = end.search(str).regs[0]
        str = str[:c]
    except AttributeError:
        pass 
    return str


class Function:

    funcNameRe = re.compile("[a-zA-Z_]+\(")
    varDeclRe = re.compile("^[ \t]*(unsigned|int|long|float|" +
                           "double|struct|enum)[^;]*")
    varNameRe = re.compile("[A-Z][A-Za-z_]*")

    def __init__(self, decl):
        a, b = self.funcNameRe.search(decl).regs[0]
        rt, self.funcname, args = decl[:a], decl[a:b-1], decl[b:]
        args = args[:string.index(args, ")")]
        self.args = arglst = [ ]
        for x in string.split(args, ","):
            self.getVars(x, arglst)
        self.contextSwitches = [ ]
        self.returnType = trimWhiteSpace(rt)
        self.entry = contextSwitchIndex

    def oneContextSwitch(self, index=None):
        global contextSwitchIndex
        if index == -1:
            return "cxr(%s);" % self.funcname
        else:
            i = contextSwitchIndex
            contextSwitchIndex = contextSwitchIndex + 1
            return "cxx(%s,%d);" % (self.funcname, i)

    def callMe(self):
        args, dict = self.args, self.__dict__
        r = "void %(funcname)s(struct context *ctxt, " % dict
        for type, name in self.args:
            r = r + "%s %s, " % (type, name)
        if self.returnType != "void":
            r = r + self.returnType + "*return_value"
        else:
            r = r[:-2]
        r = r + ")\n{\n  struct frame_%(funcname)s *p;\n" % dict
        r = r + "  int n = sizeof(struct frame_%(funcname)s);\n" % dict
        r = r + "  ctxt->stack_pointer -= n;\n" % dict
        r = (r +
             "  p = (struct frame_%(funcname)s *) ctxt->stack_pointer;\n"
             % dict)
        r = r + "  p->jump_tag = %(entry)d;\n" % dict
        for type, name in args:
            r = r + "  p->%s = %s;\n" % (name, name)
        if self.returnType != "void":
            r = r + "  p->return_value = return_value;\n"
        return r + "}\n"

    def getContextSwitches(self, str, contextSwitchRe = re.compile("cx;")):
        newstr = ""
        while 1:
            m = contextSwitchRe.search(str)
            if m == None:
                return newstr + str
            a, b = m.regs[0]
            x, y, str = str[:a], str[a:b], str[b:]
            newstr = newstr + x + self.oneContextSwitch()

    def getVars(self, str, varlst):
        v = self.varDeclRe.search(str)
        if v == None:
            return 0
        a, b = v.regs[0]
        x = string.split(str[a:b], ",")
        try:
            a, b = self.varNameRe.search(x[0]).regs[0]
            keepGoing = 1
        except AttributeError:
            keepGoing = 0
        if keepGoing:
            varType = trimWhiteSpace(x[0][:a])
            varName = trimWhiteSpace(x[0][a:])
            varlst.append((varType, varName))
            for y in x[1:]:
                varlst.append((varType, trimWhiteSpace(y)))
        return 1

    def read(self, lines):
        self.vars = vars = [ ]
        dict = self.__dict__
        _body = [ "  " + self.oneContextSwitch() ]
        i = 0
        n = len(lines)
        while lines[i][:1] != "{" and i < n:
            i = i + 1
        i = i + 1
        assert i < n
        while lines[i][:1] != "}" and i < n:
            if not self.getVars(lines[i], vars):
                _body.append(lines[i])
            i = i + 1
        _body.append("  " + self.oneContextSwitch(-1))

        _body2 = [ ]
        lst = self.args + self.vars
        for x in _body:
            x = string.replace(x,
                               "return ",
                               "*(%(funcname)s_state->return_value) = "
                               % dict)
            x = self.getContextSwitches(x)
            for (w, y) in lst:
                self.v = y
                z = "(%(funcname)s_state->%(v)s)" % dict
                x = string.replace(x, y, z)
            _body2.append(x)
        self._body = _body2

        return i

    def handleFunctionCall(self, R):
        _body = self._body
        srch = R.search
        for i in range(len(_body)):
            x = _body[i]
            try:
                a, b = srch(x).regs[0]
                keepGoing = 1
            except AttributeError:
                keepGoing = 0
            if keepGoing:
                u, v, x = x[:a], x[a:b], x[b:]
                v = trimWhiteSpace(v[:-1])
                assignment = re.compile("=[ \t]*$").search(u)
                if assignment != None:
                    a, b = assignment.regs[0]
                    u = trimWhiteSpace(u[:a])
                    a, b = re.compile("\)[ \t]*;[ \t]*$").search(x).regs[0]
                    x = x[:a] + ", &(" + u + "));"
                else:
                    a, b = re.compile("\)[ \t]*;[ \t]*$").search(x).regs[0]
                    x = x[:a] + ", NULL);"
                _body[i] = (v + "(ctxt, " + x + "\n" +
                            self.oneContextSwitch())

    def struct(self):
        struc = "struct frame_%s {\n" % self.funcname
        struc = struc + "  int jump_tag;\n"
        for (x, y) in (self.args + self.vars):
            struc = struc + "  %s %s;\n" % (x, y)
        if self.returnType != "void":
            struc = struc + "  %s *return_value;\n" % self.returnType
        return struc + "};\n"

    def body(self):
        return string.join(self._body, "\n")

    def __repr__(self):
        r = "<Function %s\n" % self.funcname
        r = r + "    args: %s\n" % repr(self.args)
        r = r + "    return type: %s\n" % self.returnType
        for x in self.vars:
            r = r + "    var: %s %s\n" % x
        return r + self.body() + " >"

#####################################################

output_file_preamble = """/*
 *  Automatically generated file, do not edit
 */

#include <stdio.h>
#include <malloc.h>

#define STACK_SIZE 100

struct context {
  unsigned char *stack_pointer;
  unsigned char stack[STACK_SIZE];
};

#define cxx(fn,n)  { fn ## _state->jump_tag = n; return 1; L ## n:; }
#define cxr(fn)  \\
{ ctxt->stack_pointer += sizeof(struct frame_ ## fn); return 1; }

struct context *new_context(void)
{
  struct context *ctxt;
  ctxt = malloc(sizeof(struct context));
  if (ctxt == NULL)
    {
      fprintf(stderr, "ouch\\n");
      exit(1);
    }
  memset(ctxt->stack, 0, STACK_SIZE);
  ctxt->stack_pointer = ctxt->stack + STACK_SIZE;
  return ctxt;
}

"""

class MTTFile:
    globalDeclRe = re.compile("^[a-zA-Z].*(/\*)?")
    parOnRe = re.compile("^#pragma[ \t]+par_on[ \t]*$")
    parOffRe = re.compile("^#pragma[ \t]+par_off[ \t]*$")

    def __init__(self, inf):
        R = string.split(inf.read(), "\n")
        self.preamble = [ ]
        self.globalFuncs = globalFuncs = [ ]
        self.parFlag = 0
        i = 0
        while i < len(R):
            x = R[i]
            # find functions and consume them
            got_it = 0
            if self.parOnRe.search(x):
                self.parFlag = 1
            elif self.parOffRe.search(x):
                self.parFlag = 0
            elif self.parFlag and self.globalDeclRe.search(x):
                try:
                    string.index(x, "(")
                    isAFunc = 1
                except ValueError:
                    isAFunc = 0
                if isAFunc:
                    f = Function(x)
                    globalFuncs.append(f)
                    i = i + 1 + f.read(R[i+1:])
                    got_it = 1
            if not got_it:
                # anything else is preamble
                self.preamble.append(x)
            i = i + 1

    def handleFunctionCalls(self):
        for f in self.globalFuncs:
            R = re.compile(f.funcname + "[ \t]*\(")
            for f2 in self.globalFuncs:
                f2.handleFunctionCall(R)

    def dump(self):
        globalFuncs = self.globalFuncs
        r = (output_file_preamble + "\n" +
             string.join(self.preamble, "\n")) + "\n"

        for f in globalFuncs:
            r = r + f.struct() + "\n"

        for f in globalFuncs:
            r = r + f.callMe() + "\n"

        r = r + "\nint thisfunc(struct context *ctxt)\n{\n"
        for f in globalFuncs:
            r = r + (("  struct frame_%(funcname)s * %(funcname)s_state =\n"
                      "    (struct frame_%(funcname)s *)" +
                      " ctxt->stack_pointer;\n")
                     % f.__dict__)

        r = r + ("if (ctxt->stack_pointer == ctxt->stack + STACK_SIZE)\n" +
                 "  return 0;")

        r = r + ("  switch (%s_state->jump_tag)\n  {\n" %
                 globalFuncs[0].funcname)
        for i in range(contextSwitchIndex):
            r = r + "    case %d: goto L%d;\n" % (i, i)
        r = r + "    default: return 0;\n  }\n"

        for f in globalFuncs:
            r = r + f.body()

        return r + "  return 1;\n}\n"

###################################################################
#
#     ./mmt.py < tryit.c | indent - > yo.c
#     gcc -Wall -c yo.c
#

if len(sys.argv) > 1:
    inf = open(sys.argv[1])
else:
    inf = sys.stdin

m = MTTFile(inf)
inf.close()

m.handleFunctionCalls()

print m.dump()
