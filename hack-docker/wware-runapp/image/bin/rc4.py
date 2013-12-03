#!/usr/bin/env python

import base64
import docopt
import getpass
import math
import random
import sys
import zlib


# Omit letters O and 1, and digits 0 and 1, to avoid confusion
upper = 'ABCDEFGHIJKLMNPQRSTUVWXYZ'
lower = 'abcdefghijkmnopqrstuvwxyz'
digits = '23456789'
chars = upper + lower + digits


def gen_password(entropy_bits):
    base_entropy = math.log(len(chars)) / math.log(2)
    nchars = int(math.ceil(entropy_bits) / base_entropy)
    return ''.join([random.choice(chars) for i in range(nchars)])


def stringToBytes(x):
    return map(ord, list(x))


def bytesToString(x):
    return ''.join(map(chr, x))


class RC4:

    def __init__(self, key, setupLoops=1):
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

    @classmethod
    def cs_enc(cls, key, plaintext):
        # ciphersaber encrypt
        salt = bytesToString([int(256 * random.random())
                              for i in range(10)])
        r = cls(key + salt, setupLoops=20)
        ciphertext = r.encrypt(zlib.compress(plaintext))
        return base64.encodestring(salt + ciphertext)

    @classmethod
    def cs_dec(cls, key, encrypted):
        # ciphersaber decrypt
        saltcipher = base64.decodestring(encrypted)
        salt = saltcipher[:10]
        ciphertext = saltcipher[10:]
        r = cls(key + salt, setupLoops=20)
        return zlib.decompress(r.encrypt(ciphertext))

############################################

"""
Here is something that would be cool. You give the thing a password and the
name of an encrypted file, and it decrypts the file and opens an editor with
the plaintext. You edit it or read it or whatever, and when you close the
editor, it encrypts the plaintext to get encrypted file contents, and the
editor window somehow closes in such a way that the plaintext is irretrievable.

Another cool thing would be to provide an encrypted storage service with RC4
done in JavaScript, and the key stored on the client with a passphrase so that
you enter the passphrase and pull down the encrypted content, and again it
decrypts into an edit window, and on closing the window it re-encrypts and
destroys the plaintext.  JavaScript is likely to keep everything in RAM so
that's probably even easier. And you could probably do an Android client as
well.
"""

options = docopt.docopt("""\
This is the beginnings of a password safe that will eventually become an
Android app assuming I manage to stay focused. This collects up all your
passwords and encrypts them in one file, for local storage or storage online.
It should be safe to store that file in questionable places like GMail or
Evernote or your browser's HTML5 localStorage.

You'll need a master password for the safe. It should be different from every
other password you use.

Usage:
  my_program test
  my_program encrypt [-i FILE] [-o FILE]
  my_program decrypt [-i FILE] [-o FILE]
  my_program password [-n BITS]
""".replace("my_program", sys.argv[0]))

if __name__ == "__main__":
    infile = (options['-i'] and open(options['FILE'].pop(0))) or sys.stdin
    outfile = (options['-o'] and open(options['FILE'].pop(0), 'w')) \
        or sys.stdout
    nbits = (options['-n'] and int(options['BITS'])) or 64

    if options['test']:
        cleartext = """\
This is a test of the RC4 algorithm operating in CipherSaber mode.
See http://en.wikipedia.org/wiki/CipherSaber for more details.
"""
        key = gen_password(64)
        x = RC4.cs_enc(key, cleartext)
        print x
        y = RC4.cs_dec(key, x)
        print y
        assert y == cleartext
        print 'Looks OK'

    elif options['encrypt']:
        key = getpass.getpass()
        confirm = getpass.getpass(prompt='Confirm: ')
        assert key == confirm, 'passwords did not match'
        plaintext = infile.read()
        outfile.write(RC4.cs_enc(key, plaintext))

    elif options['decrypt']:
        key = getpass.getpass()
        confirm = getpass.getpass(prompt='Confirm: ')
        assert key == confirm, 'passwords did not match'
        ciphertext = infile.read()
        outfile.write(RC4.cs_dec(key, ciphertext))

    elif options['password']:
        print gen_password(nbits)
