#!/usr/bin/python
# Copyright 2000 Enhanced Software Technologies Inc.
#
# Licensed as Open Source software under a BSD-style license.
# Read file 'LICENSE.ocotillo' for more information.

# This uses the 'twofish' module to encrypt and decrypt a file.
# It operates in one of two ways:
#
# keyfile mode: -- if the '-k <keyfile>' option is passed on cmd line, the next
# option is the keyfile. It contains a 128 to 256 bit key in binary. Unless
# -i or -o options are provided, all i/o is to stdin/stdout. Use 'randkey'
# program to create a random key.
#
# immediate mode -- you must provide "-i <inputfile>" argument. The output
# file will be "inputfile.enc", unless you provide a "-o <outputfile>"
# argument. You will be prompted for a passphrase. This passphrase will then
# be md5-hashed. The resulting 128-bit string is then passed to the
# encryption algorithm as its key.

# note that we're a wrapper around twofish.py. We should only be called
# from the command line.

import getopt
import hashlib
import sys
import twofish

cipher = None

def set_key_from_file(cipher,keyfilename):
    fh=open(keyfilename,"r")
    key=fh.read(32) # read a max of 256 chars!
    if len(key) > 24 and len(key) < 32:
	key=key[0:24]
    elif len(key) > 16 and len(key) < 24:
	key=key[0:16]
    elif len(key) < 16:
	sys.stderr.write("Invalid Key Length in key file %s\n" % keyfilename)
	sys.exit(1)
	return
    cipher.setkey(key)
    return

def prompt_for_key(cipher):
    m = hashlib.md5()
    line = raw_input("Passphrase: ")
    m.update(line)
    key=m.digest()
    cipher.setkey(key)
    return

def execute_encryption():
    inputfilename = None
    outputfilename = None
    keyfilename = None
    decryptFlag = False
    global cipher

    optlist, args = getopt.getopt(sys.argv[1:], "i:o:k:d")
    for (option, value) in optlist:
        if option == '-i':
            inputfilename = value
        elif option == '-o':
            outputfilename = value
        elif option == '-k':
            keyfilename = value
        elif option == '-d':
            decryptFlag = True

    # check to see if we got enough parameters
    if keyfilename is None and inputfilename is None:
	sys.stderr.write("Error: must specify either keyfile or input file.\n")
	sys.exit(1)
	return

    cipher=twofish.twofish()

    # okay, if we have a keyfilename, do that, else prompt for key:
    if keyfilename is not None:
	set_key_from_file(cipher,keyfilename)
    else:
	prompt_for_key(cipher)
	pass

    if inputfilename is not None:
	inputfile=open(inputfilename,"r")
	if outputfilename is None:
	    outputfilename=inputfilename+".tfe"
    else:
	inputfile=sys.stdin

    if outputfilename:
	outputfile=open(outputfilename,"w")
    else:
	outputfile=sys.stdout

    if decryptFlag:
        cipher.decrypt_cfb_file(inputfile, outputfile)
    else:
        cipher.encrypt_cfb_file(inputfile, outputfile)

if __name__ == '__main__':
    execute_encryption()
