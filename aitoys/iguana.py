import sys, string, re, types
import igobjs
import opnames

class Machine:
    commentRe = re.compile('#')

    def __init__(self):
        self.p = igobjs.Program()
        self.stack = [ ]
        self.words = [ ]
        self.names = { }
        self.reverse = { }
        self.knownConstants = { }
        self.constptr = 0

    def Thread(self):
        return igobjs.Thread(self.p)

    def getword(self):
        return self.words.pop(0)

    def compile_one_word(self, x):
        try:
            n = self.names[x]
            self.p.compile(n)
            return
        except KeyError:
            pass
        try:
            opnames.forward[x]
            self.p.compile(getattr(igobjs, x))
            return
        except KeyError:
            pass
        try:
            fn = getattr(self, 'hack_' + x)
            fn()
            return
        except AttributeError:
            pass
        number = eval(x)
        if type(number) == types.IntType:
            self.p.compile(igobjs.PUSHCONST)
            self.p.compile(number)
        elif type(number) == types.FloatType:
            self.p.compile(igobjs.DPUSHCONST)
            self.p.compile_double(number)
        else:
            raise 'ouch'

    def begin_forward_jump(self, jumptype):
        self.p.compile(jumptype)
        self.stack.append(self.p.compileptr)
        self.p.compile(0)

    def resolve_forward_jump(self):
        n = self.stack.pop()
        dest = self.p.compileptr
        self.p[n] = dest

    def hack_START(self):
        self.p.startaddr = self.p.compileptr

    def hack_IF(self):
        self.begin_forward_jump(igobjs.JUMPZ)

    def hack_ELSE(self):
        self.begin_forward_jump(igobjs.JUMP)
        x = self.stack.pop()
        self.resolve_forward_jump()
        self.stack.append(x)

    def hack_ENDIF(self):
        self.resolve_forward_jump()

    def hack_DEFINE(self):
        name = self.getword()
        n = self.p.compileptr
        self.names[name] = n
        self.reverse[n] = name

    def compilestring(self, str):
        commentRe = self.commentRe
        words = self.words
        for line in string.split(str, '\n'):
            m = commentRe.search(line)
            if m:
                n = m.regs[0][0]
                line = line[:n]
            for x in string.split(line):
                words.append(x)
        while words:
            self.compile_one_word(self.getword())

    def compilefile(self, filename):
        inf = open(filename)
        self.compilestring(inf.read())
        inf.close()

    def decompile(self):
        i = 0
        while i < self.p.compileptr:
            if i == self.p.startaddr:
                print '<<<< START >>>>'
            try:
                name = self.reverse[i]
                print '%-15s' % name,
            except:
                print 15 * ' ',
            x = self.p[i]
            print i, x,
            i = i + 1
            try:
                y = opnames.backward[x]
                if y == 'PUSHCONST':
                    print 'PUSHCONST', self.p[i]
                    i = i + 1
                elif y == 'DPUSHCONST':
                    print 'DPUSHCONST', self.p.read_const(i)
                    i = i + 2
                elif y == 'JUMP' or y == 'JUMPZ':
                    print y, self.p[i]
                    i = i + 1
                else:
                    print y
            except KeyError:
                try:
                    print self.reverse[x]
                except KeyError:
                    print '???'
