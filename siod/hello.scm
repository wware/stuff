#!/usr/local/bin/siod -v01,-m2 
#-*-mode:text;parser:pratt-*-
# Note: hpux seems to ignore the first line in the file if it is 
#       longer than 32 bytes hence we put the parser flag
#       on the next line.

# To compile this file, use the following command:
#
#  csiod :o=hello hello.scm
#

main() := 
{writes(nil,"Hello Scheme World.\n");
 fflush(nil);
 writes(nil,"fib(20) = ",fib(20),"\n");
}
$

fib(x) := if x < 2 then x else fib(x-1) + fib(x-2)
$
