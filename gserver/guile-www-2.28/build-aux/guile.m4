## Some -*-autoconf-*- macros for working with Guile.
##
## Copyright (C) 2002, 2003, 2004, 2005, 2007, 2008 Thien-Thi Nguyen
## Copyright (C) 1998, 2001 Free Software Foundation, Inc.
##
## This file is part of GUILE
##
## GUILE is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3, or (at your option)
## any later version.
##
## GUILE is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this software; see the file COPYING.  If not, write to
## the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
## Boston, MA 02110-1301, USA.

## Index
## -----
##
## GUILE_PROGS -- set paths to Guile interpreter, config and tool programs
## GUILE_FLAGS -- set flags for compiling and linking with Guile
## GUILE_SITE_DIR -- find path to Guile "site" directory (non-md files)
## GUILE_LIBSITE_DIR -- find path to Guile "library site" dir (md files)
## GUILE_CHECK -- evaluate Guile Scheme code and capture the return value
## GUILE_PROVIDEDP -- check if a feature symbol is provided
## GUILE_MODULE_CHECK -- check feature of a Guile Scheme module
## GUILE_MODULE_AVAILABLE -- check availability of a Guile Scheme module
## GUILE_MODULE_REQUIRED -- fail if a Guile Scheme module is unavailable
## GUILE_MODULE_EXPORTS -- check if a module exports a variable
## GUILE_MODULE_REQUIRED_EXPORT -- fail if a module doesn't export a variable
## GUILE_CHECK_ICE9_OPTARGS -- use (ice-9 optargs) for (ice-9 optargs-kw)?
## GUILE_MODULE_CATALOG_PREP -- machinery for pre-inst merged module catalog
## GUILE_TOOLS_PROG -- find absolute filename of a guile-tools program
## GUILE_C2X_METHOD -- choose "guile-tools c2x" or "guile-snarf"
## GUILE_MODSUP_H -- define HAVE_GUILE_MODSUP_H if it is indeed available

## Code
## ----

## NOTE: Comments preceding an AC_DEFUN (starting from "Usage:") are massaged
## into doc/ref/autoconf-macros.texi (see Makefile.am in that directory).

# {Program Detection}

# GUILE_PROGS -- set paths to Guile interpreter, config and tool programs
#
# Usage: GUILE_PROGS
#
# Look for programs @code{guile}, @code{guile-config} and
# @code{guile-tools}, and set variables @var{GUILE}, @var{GUILE_CONFIG} and
# @var{GUILE_TOOLS}, to their paths, respectively.  If either of the first two
# is not found, signal error.
#
# Mark the variables for substitution, as by @code{AC_SUBST}.
#
AC_DEFUN([GUILE_PROGS],
 [AC_PATH_PROG(GUILE,guile)
  if test "$GUILE" = "" ; then
      AC_MSG_ERROR([guile required but not found])
  fi
  AC_SUBST(GUILE)
  AC_PATH_PROG(GUILE_CONFIG,guile-config)
  if test "$GUILE_CONFIG" = "" ; then
      AC_MSG_ERROR([guile-config required but not found])
  fi
  AC_SUBST(GUILE_CONFIG)
  AC_PATH_PROG(GUILE_TOOLS,guile-tools)
  AC_SUBST(GUILE_TOOLS)
 ])

# {Compilation Flags Reporting}

# GUILE_FLAGS -- set flags for compiling and linking with Guile
#
# Usage: GUILE_FLAGS
#
# Run the @code{guile-config} script, installed with Guile, to
# find out where Guile's header files and libraries are installed.  Set
# two variables, @var{GUILE_CFLAGS} and @var{GUILE_LDFLAGS}.
#
# @var{GUILE_CFLAGS}: flags to pass to a C or C++ compiler to build code that
# uses Guile header files.  This is almost always just a @code{-I} flag.
#
# @var{GUILE_LDFLAGS}: flags to pass to the linker to link a program against
# Guile.  This includes @code{-lguile} for the Guile library itself, any
# libraries that Guile itself requires (like -lqthreads), and so on.  It may
# also include a @code{-L} flag to tell the compiler where to find the
# libraries.
#
# Mark the variables for substitution, as by @code{AC_SUBST}.
#
AC_DEFUN([GUILE_FLAGS],
 [AC_REQUIRE([GUILE_PROGS])dnl

  AC_CACHE_CHECK([libguile compile flags],
    [guile_cv_compile_flags],
    [guile_cv_compile_flags="`$GUILE_CONFIG compile`"])
  GUILE_CFLAGS="$guile_cv_compile_flags"

  AC_CACHE_CHECK([libguile link flags],
    [guile_cv_link_flags],
    [guile_cv_link_flags="`$GUILE_CONFIG link`"])
  GUILE_LDFLAGS="$guile_cv_link_flags"

  AC_SUBST(GUILE_CFLAGS)
  AC_SUBST(GUILE_LDFLAGS)
 ])

# {Installation Directories}

# GUILE_SITE_DIR -- find path to Guile "site" directory
#
# Usage: GUILE_SITE_DIR
#
# Look for Guile's "site" directory, usually something like
# PREFIX/share/guile/site, and set var @var{GUILE_SITE} to the path.
# Note that PREFIX is that of Guile, which may or may not coincide
# with the one specified by @code{configure --prefix}.
# Note also that the var name is different from the macro name.
#
# Mark the variable for substitution, as by @code{AC_SUBST}.
#
AC_DEFUN([GUILE_SITE_DIR],
 [AC_REQUIRE([GUILE_PROGS])dnl
  AC_CACHE_CHECK([for Guile site directory],[guile_cv_SITE],[
  # Try built-in reprefixing.
  GUILE_SITE=`[$GUILE_CONFIG] re-prefix-info scheme_site_dir 2>/dev/null`
  # If no joy, do it "manually".
  if test x"$GUILE_SITE" = x ; then
    GUILE_SITE=`[$GUILE_CONFIG] info prefix`
    GUILE_SITE=`[$GUILE_CONFIG] info pkgdatadir | sed s,$GUILE_SITE,'${prefix}',`
    GUILE_SITE="$GUILE_SITE/site"
  fi
  # If still no joy, make an educated guess.
  if test x"$GUILE_SITE" = x/site ; then
    GUILE_SITE='${datadir}/guile/site'
  fi
  guile_cv_SITE="$GUILE_SITE"])
  GUILE_SITE="$guile_cv_SITE"
  AC_SUBST(GUILE_SITE)
 ])

# GUILE_LIBSITE_DIR -- find path to Guile "library site" directory
#
# Usage: GUILE_LIBSITE_DIR
#
# Look for Guile's "library site" directory, usually something like
# PREFIX/lib/guile/site, and set var @var{GUILE_LIBSITE} to the path.
# Note that PREFIX is that of Guile, which may or may not coincide
# with the one specified by @code{configure --prefix}.
# Note also that the var name is different from the macro name.
#
# Mark the variable for substitution, as by @code{AC_SUBST}.
#
AC_DEFUN([GUILE_LIBSITE_DIR],
 [AC_REQUIRE([GUILE_PROGS])dnl
  AC_CACHE_CHECK([for Guile library site directory],[guile_cv_LIBSITE],[
  # Try built-in reprefixing.
  GUILE_LIBSITE=`[$GUILE_CONFIG] re-prefix-info pkglibdir 2>/dev/null`
  # If no joy, do it "manually".
  if test x"$GUILE_LIBSITE" = x ; then
    GUILE_LIBSITE=`[$GUILE_CONFIG] info exec_prefix`
    GUILE_LIBSITE=`[$GUILE_CONFIG] info pkglibdir | sed s,$GUILE_LIBSITE,'${exec_prefix}',`
  fi
  GUILE_LIBSITE="$GUILE_LIBSITE"/site
  # If still no joy, make an educated guess.
  if test x"$GUILE_LIBSITE" = x/site ; then
    GUILE_LIBSITE='${libdir}/guile/site'
  fi
  guile_cv_LIBSITE="$GUILE_LIBSITE"])
  GUILE_LIBSITE="$guile_cv_LIBSITE"
  AC_SUBST(GUILE_LIBSITE)
 ])

# {General Feature Checks}

# GUILE_CHECK -- evaluate Guile Scheme code and capture the return value
#
# Usage: GUILE_CHECK(var,check)
#
# Set @var{var} to the numeric return value of evaluating @var{check}.
# @var{var} is a shell variable name to be set to the return value.
#
# @var{check} is one or more Guile Scheme expression, evaluated with
# "$GUILE -c", the last of which should return either 0 or non-#f to
# indicate the check passed.  Non-0 number or #f indicates failure.
# This is conventionally achieved by wrapping the last expression in
# @code{exit}.  For example, @code{(foo) (bar) (exit (baz))}.
#
# Avoid using the character "#" since that confuses autoconf.
#
AC_DEFUN([GUILE_CHECK],
 [AC_REQUIRE([GUILE_PROGS])
  $GUILE -c "$2" > /dev/null 2>&1
  $1=$?
 ])

# GUILE_PROVIDEDP -- check if a feature symbol is provided
#
# Usage: GUILE_PROVIDEDP(var,feature)
#
# Set @var{var} based on whether or not @var{feature} is @code{provided?}.
# @var{var} is a shell variable name to be set to "yes" or "no".
# @var{feature} is a symbol, like: @code{posix}.
#
AC_DEFUN([GUILE_PROVIDEDP],
         [AC_MSG_CHECKING([if guile provides feature `$2'])
          GUILE_CHECK($1,(exit (provided? (quote $2))))
          if test "$$1" = "0" ; then $1=yes ; else $1=no ; fi
          AC_MSG_RESULT($$1)
         ])

# {Scheme Module Checks}

# GUILE_MODULE_CHECK -- check feature of a Guile Scheme module
#
# Usage: GUILE_MODULE_CHECK(var,module,featuretest,description)
#
# Set @var{var} based on whether or not @var{module} supports @var{featuretest}.
# @var{var} is a shell variable name to be set to "yes" or "no".
# @var{module} is a list of symbols, like: @code{(ice-9 common-list)}.
# @var{featuretest} is an expression acceptable to GUILE_CHECK, q.v.
# @var{description} is a present-tense verb phrase (passed to AC_MSG_CHECKING).
#
AC_DEFUN([GUILE_MODULE_CHECK],
         [AC_MSG_CHECKING([if $2 $4])
	  GUILE_CHECK($1,[(use-modules $2) (exit ((lambda () $3)))])
	  if test "$$1" = "0" ; then $1=yes ; else $1=no ; fi
          AC_MSG_RESULT($$1)
         ])

# GUILE_MODULE_AVAILABLE -- check availability of a Guile Scheme module
#
# Usage: GUILE_MODULE_AVAILABLE(var,module)
#
# Set @var{var} based on whether or not @var{module} can be found.
# @var{var} is a shell variable name to be set to "yes" or "no".
# @var{module} is a list of symbols, like: @code{(ice-9 common-list)}.
#
AC_DEFUN([GUILE_MODULE_AVAILABLE],
         [GUILE_MODULE_CHECK($1,$2,0,is available)
         ])

# GUILE_MODULE_REQUIRED -- fail if a Guile Scheme module is unavailable
#
# Usage: GUILE_MODULE_REQUIRED(symlist)
#
# Check that the module named by @var{symlist} is available.  If not, fail.
# @var{symlist} is a list of symbols, WITHOUT surrounding parens,
# like: @code{ice-9 common-list}.
#
AC_DEFUN([GUILE_MODULE_REQUIRED],
         [GUILE_MODULE_AVAILABLE(ac_guile_module_required, ($1))
          if test "$ac_guile_module_required" = "no" ; then
              AC_MSG_ERROR([required guile module not found: ($1)])
          fi
         ])

# GUILE_MODULE_EXPORTS -- check if a module exports a variable
#
# Usage: GUILE_MODULE_EXPORTS(var,module,modvar)
#
# Set @var{var} based on whether or not @var{module} exports @var{modvar}.
# @var{var} is a shell variable to be set to "yes" or "no".
# @var{module} is a list of symbols, like: @code{(ice-9 common-list)}.
# @var{modvar} is the Guile Scheme variable to check.
#
AC_DEFUN([GUILE_MODULE_EXPORTS],
 [GUILE_MODULE_CHECK($1,$2,$3,exports `$3')
 ])

# GUILE_MODULE_REQUIRED_EXPORT -- fail if a module doesn't export a variable
#
# Usage: GUILE_MODULE_REQUIRED_EXPORT(module,modvar)
#
# Check if @var{module} exports @var{modvar}.  If not, fail.
# @var{module} is a list of symbols, like: @code{(ice-9 common-list)}.
# @var{modvar} is the Guile Scheme variable to check.
#
AC_DEFUN([GUILE_MODULE_REQUIRED_EXPORT],
 [GUILE_MODULE_EXPORTS(guile_module_required_export,$1,$2)
  if test "$guile_module_required_export" = "no" ; then
      AC_MSG_ERROR([module $1 does not export $2; required])
  fi
 ])

# {Miscellaneous}

# GUILE_CHECK_ICE9_OPTARGS -- use (ice-9 optargs) for (ice-9 optargs-kw)?
#
# Usage: GUILE_CHECK_ICE9_OPTARGS(var)
#
# Check if module @code{(ice-9 optargs-kw)} is available.  If so, set
# shell var @var{var} to "no" (see why below).  Otherwise, check if
# module @code{(ice-9 optargs)} acts like @code{(ice-9 optargs-kw)}.
# If so, set @var{var} to "yes", otherwise set it to "no".
#
# Mark the variable for substitution, as by @code{AC_SUBST}.
#
# Some versions of Guile provide a module @code{(ice-9 optargs)} that
# acts like @code{(ice-9 optargs-kw)} (and subsequently omit the latter,
# instead of providing both).  Code that uses @code{(ice-9 optargs-kw)}
# solely can be textually kludged to load @code{(ice-9 optargs)} in
# these situations if @var{var} has value "yes".  Here is a Makefile.am
# fragment that demonstrates the technique:
#
# @example
# install-data-hook:
#         if test "$(need_optargs_kludge)" = yes ; then \
#            sed s/optargs-kw/optargs/ foo.scm > TMP ; \
#            mv TMP foo.scm ; \
#         fi
# @end example
#
# In this example, @var{var} is @code{need_optargs_kludge}.  If it turns
# out @code{(ice-9 optargs-kw)} is available, @code{need_optargs_kludge}
# would have value "no", and the kludge would neither be required nor
# applied.
#
AC_DEFUN([GUILE_CHECK_ICE9_OPTARGS],[
  GUILE_MODULE_AVAILABLE($1, (ice-9 optargs-kw))
  if test "$$1" = yes ; then
    $1=no
  else
    GUILE_MODULE_CHECK($1, (ice-9 optargs),
      [(= 2 ((lambda* (a #:optional b) b) 4 2))],
      [acts like (ice-9 optargs-kw)])
  fi
  AC_SUBST($1)
])

# GUILE_MODULE_CATALOG_PREP -- machinery for pre-inst merged module catalog
#
# Usage: GUILE_MODULE_CATALOG_PREP([fragname])
#
# Define two AC_CONFIG_COMMAND commands to manage the local module catalog.
#
# @table @code
# @item module-catalog-prep
# This command runs towards the tail end of a @code{configure} invocation.
# It does these actions:
#
# @itemize
# @item Create a dependencies subdirectory named by the
# shell var @code{DEPDIR}, or ".deps" if not that var is not defined.
# This step is skipped if the directory already exists.
#
# @item Create a dummy makefile fragment file in the dependencies
# directory named @file{.Pmodule-catalog}.
# This step is skipped if the file already exists.  Optional
# arg FRAGNAME specifies another filename for the makefile fragment.
#
# @item Append an @code{include} directive to @file{./Makefile}.
# This step is skipped if the directive has already been appended.
# @end itemize
#
# @item module-catalog-clean-local
# This command is intended to be run as an action in the Makefile.am
# target @code{clean-local}.  It re-initializes the dependencies file
# (ie, @var{FRAGNAME} or @file{.Pmodule-catalog} in the deps dir).
# You still need to add the module catalog's name to the Makefile.am
# var @code{CLEANFILES}.  For example:
#
# @example
# # fragment of top-level Makefile.am
# CLEANFILES = .module-catalog
# clean-local:
#         ./config.status module-catalog-clean-local
# @end example
# @end table
#
AC_DEFUN([GUILE_MODULE_CATALOG_PREP],[
  AC_CONFIG_COMMANDS([module-catalog-prep],[
    test x"$DEPDIR" = x && DEPDIR=".deps"
    test -d "$DEPDIR" || mkdir "$DEPDIR"
    prereq="$DEPDIR/m4_case([$1],[],[.Pmodule-catalog],[$1])"
    test -f "$prereq" || echo '# dummy' > "$prereq"
    grep -q "include $prereq" Makefile || echo "include $prereq" >> Makefile
  ])
  AC_CONFIG_COMMANDS([module-catalog-clean-local],[
    prereq="$DEPDIR/m4_case([$1],[],[.Pmodule-catalog],[$1])"
    test -f "$prereq" && echo '# dummy' > "$prereq"
  ])
])

# GUILE_TOOLS_PROG -- find absolute filename of a guile-tools program
#
# Usage: GUILE_TOOLS_PROG(var,program,[notfound])
#
# Set shell variable @var{var} to be the absolute filename of the
# guile-tools @var{program} if it exists, or to ":" otherwise.
# Optional third arg @var{notfound} is the value to use instead of ":".
#
# Mark the variable for substitution, as by @code{AC_SUBST}.
#
AC_DEFUN([GUILE_TOOLS_PROG],[
  AC_REQUIRE([GUILE_PROGS])dnl
  AC_CACHE_CHECK([for "guile-tools $2"],[guile_cv_$1],[
  guile_cv_$1=no
  if $GUILE_TOOLS -p PROGRAM >/dev/null 2>&1 ; then
    guile_cv_$1=`$GUILE_TOOLS -p $2`
  elif $GUILE_TOOLS | grep -q $2 ; then
    guile_cv_$1=`$GUILE_TOOLS --help | $SED '/.*cripts dir: /!d;s///'`/$2
  fi
  if test "$guile_cv_$1" && test -x "$guile_cv_$1" ; then :
  else guile_cv_$1=no ; fi])
  $1="$guile_cv_$1"
  test "$$1" = no && $1=m4_case([$3],[],[:],[$3])
  AC_SUBST($1)
])

# GUILE_C2X_METHOD -- choose "guile-tools c2x" or "guile-snarf"
#
# Usage: GUILE_C2X_METHOD(var)
#
# Set shell variable @var{var} to be either the
# absolute filename of the "guile-tools c2x" program,
# or "guile-snarf", preferring the former if it is available.
#
# Mark the variable for substitution, as by @code{AC_SUBST}.
#
AC_DEFUN([GUILE_C2X_METHOD],[
  GUILE_TOOLS_PROG([$1],[c2x],[guile-snarf])
])

# GUILE_MODSUP_H -- define HAVE_GUILE_MODSUP_H if it is indeed available
#
# Usage: GUILE_MODSUP_H
#
# Check for header <guile/modsup.h> using AC_CHECK_HEADERS.
# If found, define the cpp symbol HAVE_GUILE_MODSUP_H in config.h.
#
AC_DEFUN([GUILE_MODSUP_H],[
  AC_CHECK_HEADERS([guile/modsup.h])
])

## guile.m4 ends here
