#!/usr/bin/python

import string
import sys
from random import choice
from math import log

# Exclude a few characters to minimize confusion between
# similar-looking things: 0 1 I l

LETTERS = ('ABCDEFGHJKLMNPQRSTUVWXYZ' +
           'abcdefghijkmnpqrstuvwxyz')

DIGITS = '23456789'

PUNCTUATION = '+=_-[];:<,>./?!@#$%^&*()'

N = 8

needLetter = needDigit = needPunctuation = True

def generatePassword():
    chars = ""
    assert needDigit or needPunctuation or needLetter, \
        "You need something"
    if needLetter:
        chars += LETTERS
    if needDigit:
        chars += DIGITS
    if needPunctuation:
        chars += PUNCTUATION
    while True:
        gotDigit = gotPunctuation = gotLetter = False
        password = [choice(chars) for i in range(N)]
        for ch in password:
            if ch in LETTERS or not needLetter:
                gotLetter = True
            if ch in DIGITS or not needDigit:
                gotDigit = True
            if ch in PUNCTUATION or not needPunctuation:
                gotPunctuation = True
        if gotDigit and gotPunctuation and gotLetter:
            entropy = N * (log(len(chars)) / log(2.0))
            print 'Entropy:', entropy
            return ''.join(password)

args = sys.argv[1:]
try:
    n = args.index("-letter")
    args = args[:n] + args[n+1:]
    needLetter = False
except:
    pass
try:
    n = args.index("-digit")
    args = args[:n] + args[n+1:]
    needDigit = False
except:
    pass
try:
    n = args.index("-punc")
    args = args[:n] + args[n+1:]
    needPunctuation = False
except:
    pass
try:
    n = args.index("+confusion")
    args = args[:n] + args[n+1:]
    LETTERS += "lI"
    DIGITS += "01"
except:
    pass

try:
    N = string.atoi(args[0])
except:
    pass

print 'Password:', generatePassword()
