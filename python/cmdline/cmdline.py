#!/usr/bin/python

import getopt
import sys
import random

class CmdLine:
    # Docstrings are waffle-separated with three fields. The first field is any
    # short opts (like 'h?' for help), the second field is the long opt (if it
    # exists) and the third field is a one-line description of the option.
    #
    # If the short option has a colon, or the long option ends with an equal sign,
    # then the option consumes an argument, e.g. -o outputfile.
    options = [ ]
    commands = { }
    values = { }
    blurbtext = None
    _afterOptions = [ ]

    def __init__(self, docstr, thunk):
        self.shorts, self.long, self.descrip = docstr.split("#")
        self.thunk = thunk

    @classmethod
    def blurb(cls, text):
        cls.blurbtext = text

    @classmethod
    def setAfterOptions(cls, thunk):
        cls._afterOptions.append(thunk)

    def usage_line(self):
        arg_needed = False
        str = ""
        i = 0
        while i < len(self.shorts):
            str += ", -" + self.shorts[i]
            i += 1
            if i < len(self.shorts) and self.shorts[i] == ':':
                arg_needed = True
                str += " arg"
                i += 1
        if len(self.long) > 0:
            str += ", --" + self.long
            if str[-1:] == '=':
                assert arg_needed, "Inconsistency: " + self.long
                str = str[:-1] + " arg"
        if str[:2] == ", ":
            str = str[2:]
        str = "    " + str
        return str + (" " * max(2, 30 - len(str))) + self.descrip

    def handle(self, opt, arg):
        if ((len(opt) == 2 and opt[0] == '-' and opt[1] in self.shorts) or
            (len(opt) >= 3 and opt[:2] == '--' and opt[2:] == self.long)):
            self.thunk(arg)
            return True
        else:
            return False

    @classmethod
    def addGeneralOption(cls, docstr, thunk, default=None):
        key = docstr.split("#")[1]
        cls.values[key] = default
        cls.options.append(cls(docstr, thunk))

    @classmethod
    def flag(cls, docstr):
        key = docstr.split("#")[1]
        cls.values[key] = False
        def thunk(ignored=None, cls=cls, key=key):
            cls.values[key] = True
        cls.options.append(cls(docstr, thunk))

    @classmethod
    def addTypedArg(cls, docstr, default, castfunc):
        short, key, blab = docstr.split("#")
        # adjust short and long options to take an argument
        short = "".join(map(lambda x: x + ":", list(short)))
        docstr = short + "#" + key + "=#" + blab
        def thunk(arg, cls=cls, key=key, castfunc=castfunc):
            cls.values[key] = castfunc(arg)
        cls.addGeneralOption(docstr, thunk, default)

    @classmethod
    def stringArg(cls, docstr, default=""):
        cls.addTypedArg(docstr, default, lambda x: x)

    @classmethod
    def intArg(cls, docstr, default=0):
        cls.addTypedArg(docstr, default, lambda x: int(x))

    @classmethod
    def floatArg(cls, docstr, default=0.):
        cls.addTypedArg(docstr, default, lambda x: float(x))

    @classmethod
    def handle_all(cls, args, no_args_ok=False):
        if not (args or no_args_ok):
            cls.usage()
            sys.exit(0)

        try:
            opts, args = getopt.getopt(args,
                                       "".join(x.shorts for x in cls.options),
                                       [x.long for x in cls.options])
        except getopt.GetoptError, err:
            # print help information and exit:
            print str(err) # will print something like "option -a not recognized"
            cls.usage()
            sys.exit(2)

        recognized = (len(opts) == 0)
        for o, a in opts:
            for option in cls.options:
                if option.handle(o, a):
                    recognized = True
                    break
        assert recognized, "unhandled option"

        if cls._afterOptions:
            cls._afterOptions[0]()

        for arg in args:
            if cls.commands.has_key(arg):
                d, f = cls.commands[arg]
                f()
            else:
                print "What the heck is this??  ", arg
                print
                cls.usage()
                sys.exit(2)

    @classmethod
    def getInt(cls, key):
        return cls.values.get(key) or 0

    @classmethod
    def getFloat(cls, key):
        return cls.values.get(key) or 0.

    @classmethod
    def getString(cls, key):
        return cls.values.get(key) or ""

    @classmethod
    def getFlag(cls, key):
        return cls.values.get(key) or False

    @classmethod
    def command(cls, f):   # decorator
        d = f.__doc__
        cmd = d.split(":")[0].strip()
        cls.commands[cmd] = (d, f)

    @classmethod
    def usage(cls, ignored=None):
        print "Usage: %s [options] command command ..." % sys.argv[0]
        if cls.blurbtext:
            print
            print cls.blurbtext.strip()
        if cls.options:
            print "\nOptions:"
            for opt in cls.options:
                print opt.usage_line()
        if cls.commands.values():
            print "\nCommands:"
            for d, f in cls.commands.values():
                #print "\n".join(d.split("\n")[1:-1])
                d = d.strip()
                for line in d.split("\n"):
                    print "    " + line
        print

def _usage(ignored):
    CmdLine.usage()
    sys.exit(0)

CmdLine.options.append(CmdLine("?h#help#print useful help info", _usage))
