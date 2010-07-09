#!/usr/bin/python

#line 2 "cgrammar.y"   -- this is the kinds of comments we want

import re, sys, os, string, random

def __LINE__():
    try:
        raise "Hack"
    except:
        return sys.exc_info()[2].tb_frame.f_back.f_lineno

debugFlag = 0
infilename = outfilename = None

args = sys.argv[1:]
while args:
    arg = args.pop(0)
    if arg == "-debug":
        debugFlag = 1
    elif infilename == None:
        infilename = arg
    elif outfilename == None:
        outfilename = arg

infile = open(infilename, 'r')
R = infile.read()
infile.close()

par_on = re.compile("#pragma par_on").search
par_off = re.compile("#pragma par_off").search

a, b = par_on(R).regs[0]
c, d = par_off(R).regs[0]

R1 = R[:a]
N1 = len(string.split(R1, '\n'))  # number of lines
R2 = R[b:c]
N2 = len(string.split(R1, '\n'))  # number of lines
R3 = R[d:]

# We'll process R2, where functions are parallelized, and leave
# R1 and R3 alone. First let's strip comments.

one_line_comment = re.compile("//.*\n").search
comment_start = re.compile("/\*").search
comment_finish = re.compile("\*/").search

R2new = ""
while R2:
    try:
        a, b = one_line_comment(R2).regs[0]
        x, R2 = R2[:a] + "\n", R2[b:]
        R2new = R2new + x
    except AttributeError:
        break
R2 = R2new + R2

R2new = ""
while R2:
    try:
        a, b = comment_start(R2).regs[0]
        c, d = comment_finish(R2).regs[0]
        x, R2 = R2[:a], R2[d:]
        R2new = R2new + x
    except AttributeError:
        break
R2 = R2new + R2

# Now we want to identify the function prototypes. Each function has
# a return type, a name, and a list of 2-tuples for arguments, the
# tuples each having a type and a name. Let's generously assume that
# there are no global variables, structs, or other stuff in R2 except
# the functions we want. We'll also ignore "extern", "static",
# "volatile", or anything else similarly tricky.

class CThing:
    def __init__(self, str):
        x = string.split(str)
        self.name = x.pop()
        if len(x) == 0: x = [ 'int' ]
        self.type = string.join(x, ' ')
        while self.name[0] == '*':
            self.name = self.name[1:]
            self.type = self.type + " *"

    def __repr__(self):
        r = "<CThing %s, type: %s" % (self.name, self.type)
        for k in self.__dict__.keys():
            if k != "name" and k != "type":
                r = r + "\n%s: %s" % (k, repr(self.__dict__[k]))
        return r + ">"

    def trim_ws(self, x,
                start = re.compile("^[ \t\n]+").search,
                finish = re.compile("[ \t\n]+$").search):
        try:
            a, b = start(x).regs[0]
            x = x[b:]
        except AttributeError:
            pass
        try:
            a, b = finish(x).regs[0]
            x = x[:a]
        except AttributeError:
            pass
        return x

    def consume_until(self, x, str):
        try:
            a, b = re.search(str, x).regs[0]
            return x[:a], x[a:b], x[b:]
        except AttributeError:
            return None, None, x


class NoMoreFunctions(Exception):
    pass


call_string = """
{ struct %(name)s_frame *p = (struct %(name)s_frame *)
new_frame (%(name)s, sizeof(struct %(name)s_frame), context);
"""
# we'll close that open brace later

struct_string = """struct %(name)s_frame
{
  int (*func) (struct thread *);
  int tag, framesize;
  struct generic_frame *caller;
  struct thread *context;
  void *return_value_pointer;
"""

class Function(CThing):

    def __init__(self, R):
        x, discard, R = self.consume_until(R, "\(")
        if x == None:
            raise NoMoreFunctions
        CThing.__init__(self, x)
        arglist = [ ]
        funclocals = [ ]
        #if self.type != "void":
        #    funclocals.append(CThing(self.type + " *return_value_pointer"))
        x, discard, R = self.consume_until(R, "\)")
        for arg in string.split(x, ','):
            if arg != "void":
                arglist.append(CThing(arg))
        self.args = arglist

        # Now we're going to scan for the outermost { } brackets that
        # enclose the body of this function. We can get into trouble
        # here if the function body includes a string with unmatched
        # brackets in it, because I'm not going to go out of my mind
        # trying to parse C strings. So be warned, users, do not put
        # brackets in strings unless they match.
        funcbody = ""
        x, discard, R = self.consume_until(R, '{')
        assert len(self.trim_ws(x)) == 0
        bracketlevel = 1
        while 1:
            x, y, R = self.consume_until(R, '[{}]')
            assert x != None
            funcbody = funcbody + x
            if y == '{':
                funcbody = funcbody + y
                bracketlevel = bracketlevel + 1
            elif y == '}':
                bracketlevel = bracketlevel - 1
                if bracketlevel == 0:
                    break  # leave the while 1 loop
                funcbody = funcbody + y

        # Pull out declarations of local variables
        typelist = ["int", "long", "float", "double", "char",
                    "struct", "enum", "unsigned", "void"]
        while (len(funcbody) > 0 and
               string.split(funcbody)[0] in typelist):
            x, y, funcbody = self.consume_until(funcbody, ';')
            assert x != None
            x = string.split(x, ',')   # handle multiple declarations
            firstvar = string.split(x[0])
            vartype, varname = string.join(firstvar[:-1], ' '), firstvar[-1]
            # if there's a "*" for a pointer, it will apply only to the
            # first of the multiple declarations
            if vartype[-1] == '*':
                vartype, varname = vartype[:-1], '*' + varname
            funclocals.append(CThing(vartype + ' ' + varname))
            for varname in x[1:]:
                funclocals.append(CThing(vartype + ' ' + varname))

        self.body = funcbody
        self.locals = funclocals
        self.results = [ ]   # return values from outgoing function calls
        self._remains = R

    def remains(self):
        R = self._remains
        del self._remains
        return R

    def handle_function_call(self, func):
        body = self.body
        srch1 = re.compile(func.name + '[ \t\n]*\(').search
        srch2 = re.compile('\)').search
        start_here = 0
        while 1:
            try:
                a, b = srch1(body[start_here:]).regs[0]
            except AttributeError:
                break

            # Having identified a function call, I need to back up to
            # the previous statement (the ';' just prior to the
            # function call) or the preceding open-curly-brace or
            # close-curly-brace, and insert my special call statement
            # there. What a mess.

            before = body[:a+start_here]
            remains = body[b+start_here:]
            start_here = start_here + b
            a, b = srch2(remains).regs[0]
            args = string.split(remains[:a], ',')
            after = remains[b:]
            # Now search backwards for ';' or '{' or '}'
            i = len(before) - 1
            while i > 0:
                if before[i] == ';' or before[i] == '{' or before[i] == '}':
                    i = i + 1
                    break
                i = i - 1
            before, statement_preamble = before[:i], before[i:]
            result_var, func_call = self.setup_func_call(func, args)
            if result_var != None:
                self.results.append(result_var)
                x = (before + func_call +
                     statement_preamble + result_var.name)
            else:
                x = before + func_call + statement_preamble
            start_here = len(x)
            body = x + after
        self.body = body

    def setup_func_call(self, func, arglist):
        if arglist == ['']:
            arglist = [ ]
        D = {'name': func.name, 'result': "0" }
        if func.type == 'void':
            result = None
        else:
            resultname = '_' + `int(100000000 * random.random())`
            result = CThing(func.type + " " + resultname)
            D['result'] = "&" + resultname
        assignments = ""
        assert len(arglist) == len(func.args)
        for x, y in map(None, func.args, arglist):
            assert hasattr(x, 'name')
            assignments = (assignments +
                           "    p->%s = %s;\n" % (x.name, y))
        return result, ((call_string + assignments +
                         "p->return_value_pointer = (void*)%(result)s; CSW; }") % D)

    def frame_struct(self):
        if hasattr(self, 'no_frame_struct'):
            return ""
        r = struct_string % {'name': self.name}
        for arg in self.args:
            r = r + "  %s %s;\n" % (arg.type, arg.name)
        for local in self.locals:
            r = r + "  %s %s;\n" % (local.type, local.name)
        for result in self.results:
            r = r + "  %s %s;\n" % (result.type, result.name)
        return r + '};\n'

    def process_variables(self):
        varlist = [ ]
        for v in self.args + self.locals + self.results:
            s = re.search("\[[^]]*\]", v.name) # check if array
            if s:
                # if an array, lose dimensions, save name only
                varname = v.name[:s.regs[0][0]]
            else:
                varname = v.name
            varlist.append(varname)
        name_search = re.compile("[_a-zA-Z][_a-zA-Z0-9]*").search
        body, newbody = self.body, ""
        while 1:
            try:
                a, b = name_search(body).regs[0]
            except AttributeError:
                break
            x, y, body = body[:a], body[a:b], body[b:]
            newbody = newbody + x
            if y in varlist and x[-2:] != '->':
                newbody = newbody + "(frame->" + y + ")"
            else:
                newbody = newbody + y
        self.body = newbody + body

    def process_context_switches(self):
        csw_count = 0
        body = 'CSW;\n' + self.body
        srch = re.compile("CSW").search
        while 1:
            try:
                a, b = srch(body).regs[0]
            except AttributeError:
                break
            if csw_count > 0:
                body = (body[:a] +
                        (("{ frame->tag = %d; return 0; L%d: ; }")
                         % (csw_count, csw_count)) + body[b:])
            else:
                body = (body[:a] + " L0: " + body[b:])
            csw_count = csw_count + 1
        switchstmt = "switch (frame->tag) {\n"
        for i in range(csw_count):
            switchstmt = (switchstmt +
                          "case %d: goto L%d;\n" % (i, i))
        self.body = switchstmt + "}\n" + body

    def process_returns(self):
        body = self.body
        srch1 = re.compile("return[^_]").search
        srch2 = re.compile(";").search
        start_here = 0
        while 1:
            m = srch1(body[start_here:])
            if m == None:
                break
            a, b = srch1(body[start_here:]).regs[0]
            a = a + start_here
            b = b + start_here
            start_here = b
            c, d = srch2(body[b:]).regs[0]
            c = c + b
            d = d + b
            x = (body[:a] +
                 "if (frame->return_value_pointer!= NULL){" +
                 "*((" + self.type +
                 " *)frame->return_value_pointer) =" + body[b:c] +
                 ";} return 1;")
            start_here = len(x)
            body = x + body[d:]
        self.body = body

    def dumpfunc(self):
        if hasattr(self, 'no_dump_func'):
            return ""
        name = self.name
        R = "int %s (struct thread *context)\n{\n" % name
        R = (R +
             ("struct %s_frame *frame = (struct %s_frame *) context->frame;"
              % (name, name)))
        return R + self.body + " return 1;}\n"

class ForcedFunction(Function):

    """Provide a prototype for a function whose body
    is defined in thr.c"""

    def __init__(self, R):
        x, discard, R = self.consume_until(R, "\(")
        assert x
        CThing.__init__(self, x)
        arglist = [ ]
        funclocals = [ ]
        if self.type != "void":
            funclocals.append(CThing(self.type + " *return_value"))
        x, discard1, discard2 = self.consume_until(R, "\)")
        for arg in string.split(x, ','):
            if arg != "void":
                arglist.append(CThing(arg))
        self.args = arglist
        self.body = ""
        self.locals = [ ]
        self.results = [ ]
        self.no_dump_func = self.no_frame_struct = 1


##############################################

functions = [ ForcedFunction("int pfork(void)"),
              ForcedFunction("void pexit(void)") ]

while 1:
    try:
        f = Function(R2)
        R2 = f.remains()
        functions.append(f)
    except NoMoreFunctions:
        break

if debugFlag:
    for f in functions:
        print f

for g in functions:
    for f in functions:
        f.handle_function_call(g)

for f in functions:
    f.process_returns()
    f.process_variables()
    f.process_context_switches()

###############################################

outf = os.popen("indent -i4 - > " + outfilename, "w")

outf.write(R1 + '\n')

for f in functions:
    if f.name != 'fork':
        outf.write(f.frame_struct())

outf.write('\n')

for f in functions:
    outf.write(f.dumpfunc())

outf.write('\n' + R3)
outf.close()
