#!/usr/bin/python

# Tarball Runner is in the public domain. The twofish crypto module
# for python can be found at
# http://github.com/wware/stuff/tree/master/twofishmodule-0.7/
# Passwords do not need to be secret at the recipient's physical
# location, so use raw_input, not getpass.

import hashlib
import os
import os.path
import sys
import twofish
import urllib

__doc__ = """\
Tarball Runner is a script that downloads a tarball, decrypts it if
necessary, unpacks it, and runs its run.sh script. Example usage:

  %s http://www.foobar.com/example.tar.gz

You will then be prompted for a password. If the tarball is not
encrypted, just leave it empty and hit Enter.
""" % sys.argv[0]

os.system("rm -f /tmp/run.sh /tmp/*.tar.gz")
if len(sys.argv) > 1:
    tarballUri = sys.argv[1]
else:
    print __doc__
    sys.exit(1)

tarballName = os.path.join("/tmp", os.path.split(tarballUri)[-1])
password = raw_input("Password: ")
try:
    inputfile = urllib.urlopen(tarballUri)
except IOError:
    print "Can't find tarball at that location, please check spelling"
    sys.exit(1)
outputfile = open(tarballName, "w")
if password:
    cipher = twofish.twofish()
    m = hashlib.md5()
    m.update(password)
    key = m.digest()
    cipher.setkey(key)
    cipher.decrypt_cfb_file(inputfile,outputfile)
else:
    blocksize = 1024 * 1024
    s = inputfile.read(blocksize)
    while s:
        outputfile.write(s)
        s = inputfile.read(blocksize)
inputfile.close()
outputfile.close()
os.chdir("/tmp")
if os.system("tar xfz " + tarballName) != 0:
    print "Wrong password??"
    sys.exit(1)
os.system("/tmp/run.sh")
