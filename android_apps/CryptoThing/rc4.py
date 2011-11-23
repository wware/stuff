import base64
import random
import sys
import types
import zlib

##################################################################
#
# This is the beginnings of a password safe that will eventually
# become an Android app assuming I manage to stay focused. The
# idea here is to collect up all your passwords in one place and
# encrypt them all for local storage or storage online.
#
# You'll need a master password for the safe. Make sure it is
# different from every other password you use.
#

DEFAULT_SETUP_LOOPS = 20
USE_SALT = True

COMPRESSION_LENGTH_THRESHOLD = 100


def stringToBytes(x):
    return map(ord, list(x))


def bytesToString(x):
    return ''.join(map(chr, x))


def printBytes(ary, n=None):
    if type(ary) is types.StringType:
        ary = stringToBytes(ary)
    if n is not None:
        ary = ary[:n]
    print>>sys.stderr, " ".join([("%02X" % x) for x in ary])


class RC4:

    def __init__(self, key, setupLoops=None):
        if setupLoops is None:
            setupLoops = DEFAULT_SETUP_LOOPS
        keyBytes = stringToBytes(key)
        self.S = S = [i for i in range(256)]
        j = 0
        for m in range(setupLoops):
            for i in range(256):
                j = (j + S[i] +
                     keyBytes[i % len(keyBytes)]) % 256
                S[i], S[j] = S[j], S[i]

    def sequence(self):
        S = self.S
        i = j = 0
        while True:
            i = (i + 1) % 256
            j = (j + S[i]) % 256
            S[i], S[j] = S[j], S[i]
            t = (S[i] + S[j]) % 256
            yield S[t]

    def encrypt(self, plaintext):
        seq = self.sequence()
        return bytesToString([x ^ seq.next()
                              for x in stringToBytes(plaintext)])

#    def encrypt(self, plaintext):
#        seq = self.sequence()
#        inp = stringToBytes(plaintext)
#        printBytes(inp)
#        seq = [seq.next() for x in inp]
#        printBytes(seq)
#        result = map(lambda x, y: x ^ y, inp, seq)
#        printBytes(result)
#        return bytesToString(result)

    @classmethod
    def cs_enc(cls, key, plaintext, use_zlib=False):
        # ciphersaber encrypt
        if USE_SALT:
            salt = bytesToString([int(256 * random.random())
                                  for i in range(10)])
        else:
            salt = "\000" * 10
        r = cls(salt + key)
        if len(plaintext) > COMPRESSION_LENGTH_THRESHOLD:
            plaintext = "\001" + zlib.compress(plaintext)
        else:
            plaintext = "\000" + plaintext
        ciphertext = r.encrypt(plaintext)
        return base64.encodestring(salt + ciphertext)

    @classmethod
    def cs_dec(cls, key, encrypted, use_zlib=False):
        # ciphersaber decrypt
        saltcipher = base64.decodestring(encrypted)
        salt = saltcipher[:10]
        ciphertext = saltcipher[10:]
        r = cls(salt + key)
        plaintext = r.encrypt(ciphertext)
        if plaintext[0] != '\0':
            plaintext = zlib.decompress(plaintext[1:])
        else:
            plaintext = plaintext[1:]
        return plaintext

############################################

if __name__ == "__main__":

    cmd, key = sys.argv[1:3]
    if USE_SALT:
        salt = bytesToString([int(256 * random.random())
                              for i in range(10)])
    else:
        salt = "\000" * 10
    extendedKey = salt + key

    if 'test' == cmd:
        cleartext = (len(sys.argv) > 3) and sys.argv[3] or """\
This is a test of the RC4 algorithm operating in CipherSaber mode.
See http://en.wikipedia.org/wiki/CipherSaber for more details.
"""
        key = sys.argv[2]
        x = RC4.cs_enc(extendedKey, cleartext)
        print x
        print RC4.cs_dec(extendedKey, x)

    elif 'sequence' == cmd:
        key = sys.argv[2]
        if USE_SALT:
            salt = bytesToString([int(256 * random.random())
                                  for i in range(10)])
        else:
            salt = "\000" * 10
        sequence = RC4(extendedKey).sequence()
        printBytes([sequence.next() for i in range(20)])

    elif 'encrypt' == cmd:
        key = sys.argv[2]
        plaintext = sys.argv[3]
        print RC4.cs_enc(key, plaintext)

    elif 'decrypt' == cmd:
        key = sys.argv[2]
        ciphertext = sys.argv[3]
        print RC4.cs_dec(key, ciphertext)

    elif 'encryptstream' == cmd:
        key = sys.argv[2]
        plaintext = sys.stdin.read()
        print RC4.cs_enc(key, plaintext)

    elif 'decryptstream' == cmd:
        key = sys.argv[2]
        ciphertext = sys.stdin.read()
        print RC4.cs_dec(key, ciphertext)
