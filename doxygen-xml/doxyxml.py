"""\
Convert doxygen-generated XML into native Python data structures. Generate a
parse object for each source file, and use them to build two lists. The
first list is all the functions that are reference (however indirectly) by
main(). The second is all the functions in all the source files. Difference
those lists to see which functions can be commented out. If I'm lucky, this
will save enough space to fit everything into the LE1's flash program
memory."""

import os, string, types

class XmlElement(types.ListType):

    def __init__(self, name):
        self.name = name

    def __repr__(self, indent=0):
        ind = 4 * indent * ' '
        r = ind + '<' + self.name
        kv = [ ]
        for key, value in self.__dict__.items():
            if key[:2] != '__' and key != 'name':
                kv.append((key, value))
        if kv:
            r += ' '
            r += ', '.join('%s=%s' % (key, repr(value))
                           for key, value in kv)
        if len(self) == 0:
            return r + '>'
        r += '\n'
        for x in self:
            r += x.__repr__(indent + 1) + '\n'
        return r + ind + '>'

class CSourceFile:
    # preserve as much information as possible at this level

    @classmethod
    def parseDoxyXmlFiles(cls, dirs):
        if type(dirs) in (types.StringType, types.UnicodeType):
            dirs = [ dirs ]
        lst = [ ]
        for d in dirs:
            for cfile in os.popen("/bin/ls -1 %s/*.c" % d).readlines():
                cfile = cfile.strip()
                lst.append(cls(cfile,
                               d + '/xml/' +
                               cfile.replace("_", "__")\
                                   .replace(".c", "_8c.xml")))
        return lst

    def __init__(self, cfile, xfile):
        from xml.sax.handler import ContentHandler
        from xml.sax import make_parser

        self.cfile = cfile
        self.xfile = xfile

        class ParseStackEmpty(Exception):
            def __init__(self, value):
                self.value = value

        class DoxygenHandler(ContentHandler):
            def __init__(self):
                self.stack = [ ]
                self.chars = ""
            def startElement(self, name, attrs):
                element = XmlElement(str(name))
                for key, value in attrs.items():
                    if type(value) is types.UnicodeType:
                        value = str(value)
                    setattr(element, str(key), value)
                self.stack.append(element)
            def endElement(self, name):
                this = self.stack.pop()
                if len(self.stack) == 0:
                    raise ParseStackEmpty(this)
                self.stack[-1].append(this)
            def characters(self, content,
                           # xml tags where we see meaninful text
                           texty_things = ("argsstring",
                                           "compoundname",
                                           "declname",
                                           "definition",
                                           "defname",
                                           "includedby",
                                           "includes",
                                           "initializer",
                                           "innerclass",
                                           "label",
                                           "name",
                                           "para",
                                           "parametername",
                                           "referencedby",
                                           "references",
                                           "type")):
                if len(self.stack) > 0:
                    top = self.stack[-1]
                    if top.name in texty_things:
                        if not hasattr(top, 'text'):
                            top.text = ""
                        top.text += str(content)

        saxparser = make_parser()
        saxparser.setContentHandler(DoxygenHandler())
        try:
            saxparser.parse(open(xfile))
        except ParseStackEmpty, pse:
            self.value = pse.value

    def functions(self):
        functionList = [ ]
        assert len(self.value) == 1
        for x in self.value:
            for y in x:
                for z in y:
                    if not hasattr(z, 'kind') or \
                            z.kind != 'function':
                        continue
                    f = Function.fromFileInfo(z, self.cfile)
                    if f is not None and f not in functionList:
                        functionList.append(f)
        return functionList

    def dump(self):
        N = 80
        cfile = self.cfile
        n = len(cfile)
        A = N * "*"
        B = "***" + ((N-6) * " ") + "***"
        print A; print A
        print B; print B
        U = (N - 6 - n) / 2
        V = N - 6 - n - U
        print "***" + (U * " ") + cfile + (V * " ") + "***"
        print B; print B
        print A; print A
        print self.value
        print; print; print


class JavaSourceFile:
    # preserve as much information as possible at this level

    @classmethod
    def parseDoxyXmlFiles(cls, dirs):
        if type(dirs) in (types.StringType, types.UnicodeType):
            dirs = [ dirs ]
        lst = [ ]
        for d in dirs:
            for jfile in os.popen("(cd %s ; /bin/ls -1 *.java)"
                                  % d).readlines():
                jfile = jfile.strip()
                lst.append(cls(jfile,
                               d + '/xml/' +
                               jfile.replace("_", "__")\
                                   .replace(".java", "_8java.xml")))
            for xfile in os.popen("(cd %s/xml; /bin/ls -1 class*.xml)"
                                  % d).readlines():
                lst.append(cls(None, (d + '/xml/' + xfile).strip()))
        return lst

    def __init__(self, jfile, xfile):
        from xml.sax.handler import ContentHandler
        from xml.sax import make_parser

        self.jfile = jfile
        self.xfile = xfile

        class ParseStackEmpty(Exception):
            def __init__(self, value):
                self.value = value

        class DoxygenHandler(ContentHandler):
            def __init__(self):
                self.stack = [ ]
                self.chars = ""
            def startElement(self, name, attrs):
                element = XmlElement(str(name))
                for key, value in attrs.items():
                    if type(value) is types.UnicodeType:
                        value = str(value)
                    setattr(element, str(key), value)
                self.stack.append(element)
            def endElement(self, name):
                this = self.stack.pop()
                if len(self.stack) == 0:
                    raise ParseStackEmpty(this)
                self.stack[-1].append(this)

            # grep -e '>[a-zA-Z]' *.xml | sed 's/>[a-zA-Z].*//' | \
            #     sed 's/.*<//' | sed 's/ .*//' | sort | uniq

            def characters(self, content,
                           # xml tags where we see meaninful text
                           texty_things = ("argsstring",
                                           "basecompoundref",
                                           "compoundname",
                                           "declname",
                                           "definition",
                                           "defname",
                                           "derivedcompoundref",
                                           "edgelabel",
                                           "includedby",
                                           "includes",
                                           "initializer",
                                           "innerclass",
                                           "innernamespace",
                                           "label",
                                           "name",
                                           "para",
                                           "parametername",
                                           "ref",
                                           "referencedby",
                                           "references",
                                           "reimplementedby",
                                           "reimplements",
                                           "scope",
                                           "type")):
                if len(self.stack) > 0:
                    top = self.stack[-1]
                    if top.name in texty_things:
                        if not hasattr(top, 'text'):
                            top.text = ""
                        top.text += str(content)

        saxparser = make_parser()
        saxparser.setContentHandler(DoxygenHandler())
        try:
            saxparser.parse(open(xfile))
        except ParseStackEmpty, pse:
            self.value = pse.value

    def functions(self):
        functionList = [ ]
        assert len(self.value) == 1
        for x in self.value:
            for y in x:
                for z in y:
                    if not hasattr(z, 'kind') or \
                            z.kind != 'function':
                        continue
                    f = Function.fromFileInfo(z, self.jfile)
                    if f is not None and f not in functionList:
                        functionList.append(f)
        return functionList

    def dump(self):
        N = 80
        if self.jfile is not None:
            jfile = self.jfile
        else:
            jfile = self.xfile
        n = len(jfile)
        A = N * "*"
        B = "***" + ((N-6) * " ") + "***"
        print A; print A
        print B; print B
        U = (N - 6 - n) / 2
        V = N - 6 - n - U
        print "***" + (U * " ") + jfile + (V * " ") + "***"
        print B; print B
        print A; print A
        print self.value
        print; print; print


class FunctionList(types.ListType):

    def __init__(self, filelist):
        if isinstance(filelist[0], CSourceFile):
            for file in filelist:
                for fn in file.functions():
                    if fn not in self:
                        self.append(fn)
        elif isinstance(filelist[0], Function):
            self += filelist

    def find(self, fname):
        candidates = filter(lambda x: x.name == fname, self)
        assert len(candidates) == 1, "expected exactly one " + fname
        return candidates[0]

    def extendedCallTree(self, functionList):
        # almost graph theory
        while 1:
            refs = [ ]
            for f in self:
                for x in f.lookupRefs(functionList):
                    if x not in refs and x not in self:
                        refs.append(x)
            if len(refs) == 0:
                return
            self += refs

class Function:

    # It would be way cool to be able to do a compile and link, and
    # back-annotate each function with its compiled code size.

    @classmethod
    def fromFileInfo(cls, info, cfile):
        x = cls()
        implFound = False
        # find function implementation if possible
        for u in info:
            if u.name == 'location' and hasattr(u, 'bodyfile'):
                x.implementation = {'file': u.bodyfile,
                                    'start': string.atoi(u.bodystart),
                                    'end': string.atoi(u.bodyend)}
                implFound = True
                break
        if not implFound:
            # we don't want function prototypes, just implementations
            return None

        # strip off filename portion of id, leaving unique part
        x.id = info.id[info.id.rindex('_')+1:]
        x.name = filter(lambda u: u.name == 'name', info)[0].text
        x.cfile = cfile

        # avoid duplicates
        refs = [ ]
        for ref in filter(lambda u: u.name == 'references',
                          info):
            rid = ref.refid
            rid = rid[rid.rindex('_')+1:]
            if rid not in refs:
                refs.append(rid)
        x.refs = refs

        # try to get function parameters
        def prm(ulist, x=x):
            w = { }
            declname = filter(lambda v: v.name == 'declname', ulist)
            if len(declname) == 1:
                w['name'] = declname[0].text
            ptype = filter(lambda v: v.name == 'type' and \
                               hasattr(v, 'text'),
                           ulist)
            if len(ptype) == 1:
                w['type'] = ptype[0].text
            return w
        x.params = map(prm, filter(lambda u: u.name == 'param', info))

        return x

    def __cmp__(self, other):
        r = cmp(self.cfile, other.cfile)
        if r != 0:
            return r
        return cmp(self.name, other.name)

    def __eq__(self, other):
        return self.id == other.id

    def lookupRefs(self, fcnlist):
        return filter(lambda x: x.id in self.refs,
                      fcnlist)

    def __repr__(self):
        # other fields of interest: id, params, implementation, refs
        return self.cfile + ' ' + self.name
        #return self.name
