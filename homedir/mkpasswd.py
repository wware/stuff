#!/usr/bin/python

from random import choice
from math import log

# Exclude a few characters to minimize confusion between
# similar-looking things: 0 1 I l

LETTERS = ('ABCDEFGHJKLMNPQRSTUVWXYZ' +
           'abcdefghijkmnpqrstuvwxyz')

DIGITS = '23456789'

PUNCTUATION = '+=_-[];:<,>./?!@#$%^&*()'

CHARS = LETTERS + DIGITS + PUNCTUATION

N = 8

def generatePassword():
    while True:
        gotDigit = gotPunctuation = gotLetter = False
        password = [choice(CHARS) for i in range(N)]
        for ch in password:
            if ch in LETTERS:
                gotLetter = True
            if ch in DIGITS:
                gotDigit = True
            if ch in PUNCTUATION:
                gotPunctuation = True
        if gotDigit and gotPunctuation and gotLetter:
            return ''.join(password)


#entropy = N * (log(len(CHARS)) / log(2.0))
#print 'Entropy:', entropy

print 'Password:', generatePassword()
