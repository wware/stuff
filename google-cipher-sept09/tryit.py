import string, random

cipher = \
    "8MLDQ6 T UI" + \
    "6TFML RH AA" + \
    "NRA6Q 8EFL " + \
    "DMQ86II2 O3" + \
    "2S5J 13JXOJ"

cipherWords = cipher.split(" ")
#print cipherWords

cipherLetters = { }
for w in cipherWords:
    for L in list(w):
        if cipherLetters.has_key(L):
            cipherLetters[L] += 1
        else:
            cipherLetters[L] = 1
#print cipherLetters
CLK = cipherLetters.keys()

# Let's assume it's a substitution cipher. They've got letters and digits
# but no punctuation. Let's assume for now that the real message has digits
# and letters in it. We'll also assume that a space is a space.

ABC123 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
realWords = { }
for w in open("/usr/share/dict/words").readlines():
    w = string.upper(w.strip())
    if len(w) > 0:
        # eliminate anything other than letters
        if reduce(lambda prevFlag, newChar:
                      prevFlag and (newChar in ABC123),
                  w, True):
            n = len(w)
            if realWords.has_key(n):
                realWords[n].append(w)
            else:
                realWords[n] = [ w ]

#for w in realWords[7]:
#    if w[0] == w[1] == w[4]:
#        print w

#eights = [ ]
#for w in realWords[8]:
#    if w[5] == w[6]:
#        eights.append(w)
#
#for e in eights:
#    q_is = e[2]
#    six_is = e[4]
#    for w in realWords[7]:
#        if w[5] == six_is and w[6] == q_is and w[0] == w[1]:
#            print e, w

sixes = realWords[6]
sevens = realWords[7]
eights = realWords[8]

print cipherWords

class BadMatch(Exception):
    pass

def testWord(cword, dprev, k):
    n = len(cword)
    for w1 in realWords[n]:
        try:
            # test the theory that w1 is the correct translation
            d = dprev.copy()
            d[k] = w1
            for i in range(n):
                c1 = cword[i]
                c2 = w1[i]
                if d.has_key(c1):
                    if d[c1] != c2:
                        # throw out this possiblity
                        raise BadMatch
                else:
                    if c2 in d.values():
                        raise BadMatch
                    d[c1] = c2
            # Having gotten this far, we can try another word
            yield d
        except BadMatch:
            pass

possibilities = [ { } ]

for k in (6, 2, 0):
    p2 = [ ]
    for d in possibilities:
        for d2 in testWord(cipherWords[k], d, k):
            p2.append(d2)
    possibilities = p2

    print len(possibilities)
    print possibilities[:3]
