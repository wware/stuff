#!/usr/bin/env python

# Prerequisites for Ubuntu 10.04 Lucid:
# sudo apt-get install libgmp3-dev libmpfr-dev texinfo libncurses5-dev
# probably there are more that I haven't found yet

import os, sys

root = os.popen('pwd').read().strip()
srcdir = os.path.join(root, 'src')
builddir = os.path.join(root, 'build')
objtype = 'elf'

def do_cmd(x):
    print x
    assert os.system(x) == 0

def mkdir(d):
    do_cmd('mkdir -p ' + d)

mkdir(srcdir)
mkdir(builddir)

class Package:

    def __init__(self, name, version, url):
        self.name = name
        self.version = version
        self.url = url.replace('VERSION', version)
        self.filename = os.path.split(self.url)[1]
        if name == 'gdb' and version.endswith('a'):
            version = version[:-1]
        self.srcdir = os.path.join(srcdir, name + '-' + version)
        self.blddir = os.path.join(builddir, name + '-' + version)

    def clean(self):
        do_cmd('rm -rf ' + self.srcdir)
        do_cmd('rm -rf ' + self.blddir)

    def get_source(self):
        path = os.path.join(srcdir, self.filename)
        if not os.path.isfile(path):
            do_cmd('curl -L -o %s %s' % (path, self.url))
            assert os.path.isfile(path)

    def unpack_source(self):
        path = os.path.join(srcdir, self.filename)
        if self.url.endswith('.gz'):
            cmdopts = 'zxvf'
        else:
            assert self.url.endswith('.bz2')
            cmdopts = 'jxvf'
        cmd = 'tar %s %s -C %s' % (cmdopts, path, srcdir)
        do_cmd(cmd)

    def build_phase1(self, arch):
        mkdir(self.blddir)
        opts = getattr(arch, self.name + 'opts')
        cmd = '(cd %s; %s/configure %s)' % (self.blddir,
                                            self.srcdir,
                                            opts)
        do_cmd(cmd)
        if self.name == 'gcc':
            cmd = '(cd %s; make all-gcc install-gcc)' % self.blddir
        else:
            cmd = '(cd %s; make all install)' % self.blddir
        do_cmd(cmd)

    def build_phase2(self, arch):
        if self.name == 'gcc':
            cmd = '(cd %s; make all install)' % self.blddir
            do_cmd(cmd)
        

binutils = Package('binutils',
                   '2.19.1',
                   'http://ftp.gnu.org/gnu/binutils/' +
                   'binutils-VERSION.tar.bz2')

gcc = Package('gcc',
              '4.4.0',
              'http://ftp.gnu.org/pub/gnu/gcc/' +
              'gcc-VERSION/gcc-core-VERSION.tar.bz2')

newlib = Package('newlib',
                 '1.17.0',
                 'ftp://sources.redhat.com/pub/' +
                 'newlib/newlib-VERSION.tar.gz')

gdb = Package('gdb',
              '6.8a',
              'ftp://ftp.gnu.org/gnu/gdb/gdb-VERSION.tar.bz2')

# build order
packages = [ binutils, gcc, newlib, gdb ]


class Architecture:
    def __init__(self, name):
        self.name = name
        self.prefix = '/opt/gnu-' + name
        self.gccopts = '''--target=TARGET-OBJTYPE \
            --prefix=PREFIX \
            --with-gmp=/usr/lib \
            --with-mpfr=/usr/lib \
            --enable-interwork \
            --enable-multilib \
            --with-float=soft \
            --nfp \
            --enable-languages="c" \
            --with-newlib \
            --with-headers=SRCDIR/NEWLIBDIR/newlib/libc/include'''\
            .replace('OBJTYPE', objtype)\
            .replace('TARGET', name)\
            .replace('SRCDIR', srcdir)\
            .replace('PREFIX', self.prefix)\
            .replace('NEWLIBDIR', newlib.blddir)
        self.newlibopts = '''--target=TARGET-OBJTYPE \
            --prefix=PREFIX \
            --disable-werror \
            --enable-interwork \
            --enable-multilib \
            --with-float=soft'''\
            .replace('OBJTYPE', objtype)\
            .replace('TARGET', name)\
            .replace('PREFIX', self.prefix)
        self.gdbopts = self.newlibopts
        self.binutilsopts = '''--target=TARGET-OBJTYPE \
            --prefix=PREFIX \
            --disable-werror \
            --enable-interwork \
            --enable-multilib \
            --with-float=soft'''\
            .replace('OBJTYPE', objtype)\
            .replace('TARGET', name)\
            .replace('PREFIX', self.prefix)


if 'm68k' in sys.argv[1:]:
    arch = Architecture('m68k')
else:
    arch = Architecture('arm')



if 'clean' in sys.argv[1:]:
    map(lambda pkg: pkg.clean(),
        packages)
    sys.exit(0)

mkdir(arch.prefix)

map(lambda pkg: pkg.get_source(),
    packages)

map(lambda pkg: pkg.unpack_source(),
    packages)

map(lambda pkg: pkg.build_phase1(arch),
    packages)

map(lambda pkg: pkg.build_phase2(arch),
    packages)
