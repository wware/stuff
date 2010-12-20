#!/bin/sh
#
# Copyright (c) 2008 the NxOS developers
# Subsequently modified by Will Ware.
#
# See AUTHORS for a full list of the developers.
#
# Redistribution of this file is permitted under
# the terms of the GNU Public License (GPL) version 2.
#
# Build an ARM cross-compiler toolchain (including binutils, gcc and
# newlib) on autopilot.

ROOT=`pwd`
SRCDIR=$ROOT/src
BUILDDIR=$ROOT/build
#PREFIX=$ROOT/install
PREFIX=/opt/gnu-arm

GCC_URL=http://ftp.gnu.org/pub/gnu/gcc/gcc-4.4.0/gcc-core-4.4.0.tar.bz2
GCC_VERSION=4.4.0
GCC_DIR=gcc-$GCC_VERSION

BINUTILS_URL=http://ftp.gnu.org/gnu/binutils/binutils-2.19.1.tar.bz2
BINUTILS_VERSION=2.19.1
BINUTILS_DIR=binutils-$BINUTILS_VERSION

NEWLIB_URL=ftp://sources.redhat.com/pub/newlib/newlib-1.17.0.tar.gz
NEWLIB_VERSION=1.17.0
NEWLIB_DIR=newlib-$NEWLIB_VERSION

GDB_URL=ftp://ftp.gnu.org/gnu/gdb/gdb-6.8.tar.bz2
GDB_VERSION=6.8
GDB_DIR=gdb-$GDB_VERSION

echo "I will build an arm-elf cross-compiler:

  Prefix: $PREFIX
  Sources: $SRCDIR
  Build files: $BUILDDIR

  Software: Binutils $BINUTILS_VERSION
            Gcc $GCC_VERSION
            Newlib $NEWLIB_VERSION
            Gdb $GDB_VERSION

"

#
# Helper functions.
#
ensure_source()
{
    URL=$1
    FILE=$(basename $1)

    if [ ! -e $FILE ]; then
        curl -L -O$FILE $URL
    fi
}

unpack_source()
{
(
    cd $SRCDIR
    ARCHIVE_SUFFIX=${1##*.}
    if [ "$ARCHIVE_SUFFIX" = "gz" ]; then
      tar zxvf $1
    elif [ "$ARCHIVE_SUFFIX" = "bz2" ]; then
      tar jxvf $1
    else
      echo "Unknown archive format for $1"
      exit 1
    fi
)
}

# Create all the directories we need.
mkdir -p $SRCDIR $BUILDDIR $PREFIX

(
cd $SRCDIR

# First grab all the source files...
ensure_source $GCC_URL
ensure_source $BINUTILS_URL
ensure_source $NEWLIB_URL
#rboissat: Adding GNU gdb
ensure_source $GDB_URL

# ... And unpack the sources.
unpack_source $(basename $GCC_URL)
unpack_source $(basename $BINUTILS_URL)
unpack_source $(basename $NEWLIB_URL)
unpack_source $(basename $GDB_URL)
)

# Set the PATH to include the binaries we're going to build.
OLD_PATH=$PATH
export PATH=$PREFIX/bin:$PATH

#
# Stage 1: Build binutils
#
(
mkdir -p $BUILDDIR/$BINUTILS_DIR
cd $BUILDDIR/$BINUTILS_DIR

$SRCDIR/$BINUTILS_DIR/configure --target=arm-elf --prefix=$PREFIX \
    --disable-werror --enable-interwork --enable-multilib --with-float=soft \
    && make all install
) || exit 1

#
# Stage 2: Patch the GCC multilib rules, then build the gcc compiler only
#
(
MULTILIB_CONFIG=$SRCDIR/$GCC_DIR/gcc/config/arm/t-arm-elf

echo "

MULTILIB_OPTIONS += mno-thumb-interwork/mthumb-interwork
MULTILIB_DIRNAMES += normal interwork

" >> $MULTILIB_CONFIG

mkdir -p $BUILDDIR/$GCC_DIR
cd $BUILDDIR/$GCC_DIR

$SRCDIR/$GCC_DIR/configure --target=arm-elf --prefix=$PREFIX \
    --with-gmp=/usr/lib --with-mpfr=/usr/lib \
    --enable-interwork --enable-multilib --with-float=soft --nfp \
    --enable-languages="c" --with-newlib \
    --with-headers=$SRCDIR/$NEWLIB_DIR/newlib/libc/include \
    && make all-gcc install-gcc
) || exit 1

#
# Stage 3: Build and install newlib
#
(
# And now we can build it.
mkdir -p $BUILDDIR/$NEWLIB_DIR
cd $BUILDDIR/$NEWLIB_DIR

$SRCDIR/$NEWLIB_DIR/configure --target=arm-elf --prefix=$PREFIX \
    --enable-interwork --enable-multilib --with-float=soft \
    && make all install
) || exit 1

#
# Stage 4: Build and install the rest of GCC.
#
(
cd $BUILDDIR/$GCC_DIR

make all install
) || exit 1

#
# Stage 5: Build and install GDB
#
(
mkdir -p $BUILDDIR/$GDB_DIR
cd $BUILDDIR/$GDB_DIR

$SRCDIR/$GDB_DIR/configure --target=arm-elf --prefix=$PREFIX \
    --disable-werror --enable-interwork --enable-multilib --with-float=soft \
    && make all install
) || exit 1

export PATH=$OLD_PATH

echo "export PATH=$PREFIX/bin:\$PATH">$ROOT/env.sh

echo "
Build complete! To use your new toolchain:

  - Source the $ROOT/env.sh script in your shell. In bash:
      source $ROOT/env.sh

  - Or, just add $PREFIX/bin to your PATH manually.

"
