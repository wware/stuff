import os
import sys

import cryptfile
import decryptfile

# When I get less lazy, I'll make it do correct getopt stuff.
sys.argv = [sys.argv[0],
            "-i", "cryptfile.py",
            "-k", "README",
            "-o", "foo.bar"]

cryptfile.execute_encryption()

assert open("cryptfile.py").read() != open("foo.bar").read()

sys.argv = [sys.argv[0],
            "-d",
            "-i", "foo.bar",
            "-k", "README",
            "-o", "questionable.py"]

cryptfile.execute_encryption()

assert open("cryptfile.py").read() == open("questionable.py").read()

# shameless Linux bigotry
os.system("rm -f foo.bar questionable.py")

# This is NOT a test that the cryptography is correct and secure,
# merely that decrypt(encrypt(X)) == X.
print "Huzzah! It worked."
