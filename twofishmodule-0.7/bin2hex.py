# Python!
# Copyright 1999 Enhanced Software Technologies Inc.
# All rights reserved.
#
# The purpose of this routine is to convert between binary and hexidecimal
# for big blocks of binary data stored as strings.
#
# RCS CHANGE LOG
# Revision 1.1  2000/08/14 19:30:53  eric
# added files to CVS archive
#
# Revision 1.1  2000/05/10 16:57:12  eric
# Changed all binhex to bin2hex to avoid conflict w/Mac 'binhex' routines.
#
# Revision 1.2  1999/10/28 16:43:09  eric
# Added copyright notice and RCS change log
#

hexchars="0123456789ABCDEF"

import string

# unfortunately, the mpz routines are little-endian, while we use the
# Internet big-endian order as our standard. mpz must go!
def reverse(str):
    retvalue=""
    for s in str:
        retvalue=s+retvalue
        pass
    return retvalue


# Compare this to its 'c' equivalent in bin2hex.c...
def dit2hex(ch):
    i=ord(ch) # get its integral value.
    leftnib=i>>4
    rightnib=i & 0xf
    leftchar=hexchars[leftnib]
    rightchar=hexchars[rightnib]
    return leftchar+rightchar

# compare this to its 'c' equivalent in bin2hex.c...
def bin2hex(str):
    retvalue=""  # return value is empty until we add to it...
    for s in str:
        hexchars=dit2hex(s)
        retvalue=retvalue+hexchars
        pass
    return retvalue


# this is similar to the 'c' equivalent.
def hex2dit(ch):
    if (ch >= '0') and (ch <= '9'):
        return ord(ch)-ord('0')
    if (ch >= 'a') and (ch <= 'f'):
        return ord(ch)-ord('a')+10
    if (ch >= 'A') and (ch <= 'F'):
        return ord(ch)-ord('A')+10
    hexval=bin2hex(ch)
    print "Invalid hex digit '%s' (hex value %s)." % (ch,hexval)
    raise "INVALID_HEX_DIGIT"

# turn a big-endian hex value into a string of binary values. put a leading
# zero on this guy if it doesn't have one so that we always have an even
# number of digits. Also strip any white space off the front & back first!
def hex2bin(str):
    ###DEBUG###
    # print str
    str=string.strip(str) # zap white space.
    if (not str):
	return None #agh!!!! NADA THERE!!!
    i=len(str)
    if (i % 2) != 0:
        str="0"+str  # make it an even number of digits.
        pass
    rightnib=None
    leftnib=None
    retvalue=""
    for s in str:
        if leftnib == None:
            leftnib=hex2dit(s)
        elif rightnib == None:
            rightnib=leftnib << 4
            leftnib=hex2dit(s)
        else:
            i=rightnib | leftnib
            retvalue=retvalue+chr(i)
            leftnib=hex2dit(s)
            rightnib=None
            pass
        pass
    i = rightnib | leftnib # we assured that it was even up above, so ...
    retvalue=retvalue+chr(i)

    # we should now have a return value!
    return retvalue

