#!/usr/bin/python

################################################
#
#   Here is a dumb little inference engine.
#

import sys, types

allFlag = False
debugFlag = False
def dbgprint(str, obj=None):
    if debugFlag:
        if obj is not None:
            print str + ": " + repr(obj)
        else:
            print str

class Symbol:
    def __init__(self, name):
        self.name = name
    def __repr__(self):
        return self.name
    def isVariable(self):
        return False

class Variable(Symbol):
    def isVariable(self):
        return True

def genvar(varcount=[0]):
    n = varcount[0]
    varcount[0] = n + 1
    return Variable("X%05d" % n)

IfAndOnlyIf = Symbol("ifAndOnlyIf")
Implies = Symbol("implies")
Not = Symbol("not")
And = Symbol("and")
Or = Symbol("or")
IsA = Symbol("isA")

P = Variable("p")
Q = Variable("q")
R = Variable("r")
S = Variable("s")
W = Variable("w")
X = Variable("x")
Y = Variable("y")
Z = Variable("z")

# Principles of logic are a separate category from hypotheses, because they can
# easily lead to stupid explosions of logic with infinitely long statements.
principles_of_logic = [
    [Implies,
     [IfAndOnlyIf, X, Y],
     [Implies, X, Y]],
    [Implies,
     [IfAndOnlyIf, X, Y],
     [Implies, Y, X]],
    # Contrapositive
    [Implies,
     [Implies, X, Y],
     [Implies, [Not, Y], [Not, X]]],
    # Contrapositive of contrapositive
    [Implies,
     [Not, [Implies, [Not, Y], [Not, X]]],
     [Not, [Implies, X, Y]]],
    #...
    #[IfAndOnlyIf, [Not, [And, X, Y]], [Or, [Not, X], [Not, Y]]],
    #[IfAndOnlyIf, [Not, [Or, X, Y]], [And, [Not, X], [Not, Y]]],
    [Implies, [Not, [And, X, Y]], [Or, [Not, X], [Not, Y]]],
    [Implies, [Not, [Or, X, Y]], [And, [Not, X], [Not, Y]]],
    ]

# Two statements are equivalent if they differ by only a change of
# variables.
def equivalent(stmt1, stmt2):
    def equivHelper(stmt1, stmt2, mapping):
        if type(stmt1) is types.ListType:
            if type(stmt2) is not types.ListType:
                return False
            if len(stmt1) != len(stmt2):
                return False
            for x, y in map(None, stmt1, stmt2):
                if not equivHelper(x, y, mapping):
                    return False
            return True
        if not stmt1.isVariable():
            return stmt1 is stmt2
        if mapping.has_key(stmt1):
            return stmt2 is mapping[stmt1]
        mapping[stmt1] = stmt2
        return True
    return equivHelper(stmt1, stmt2, { })

class PatternMatch:
    def __init__(self, pattern=None, data=None):
        self.map = { }
        if data is None:
            assert pattern is None
        # If the pattern and data both refer to variable X, we need
        # to remember they are two different X's. Replace any variables in
        # data with new variables, and be consistent throughout.
        def traversal(subdata,mapping):
            if subdata is None:
                return subdata
            if type(subdata) is types.ListType:
                return map(lambda x: traversal(x, mapping),
                           subdata)
            if not subdata.isVariable():
                return subdata
            if not mapping.has_key(subdata):
                mapping[subdata] = genvar()
            return mapping[subdata]
        data = traversal(data, { })
        self.match(pattern, data)
    def __repr__(self):
        return "PatternMatch" + repr(self.map)
    def match(self, pattern, data):
        if pattern == data:
            return
        if type(pattern) is types.ListType:
            assert type(data) is types.ListType
            assert len(pattern) == len(data)
            for x, y in map(None, pattern, data):
                self.match(x, y)
            return
        if pattern.isVariable():
            self.map[pattern] = data
            return
        raise Exception
    def subst(self, pattern):
        if type(pattern) != types.ListType:
            if self.map.has_key(pattern):
                return self.map[pattern]
            else:
                return pattern
        return map(lambda x: self.subst(x),
                   pattern)
    def consistentWith(self, other):
        sm = self.map
        om = other.map
        for k, v in sm.items():
            if om.has_key(k) and om[k] != sm[k]:
                return False
        return True

def noDoubleNegatives(stmt):
    if type(stmt) is not types.ListType:
        return stmt
    if (len(stmt) == 2 and
        stmt[0] is Not and
        type(stmt[1]) is types.ListType and
        len(stmt[1]) == 2 and
        stmt[1][0] is Not):
        return stmt[1][1]
    return map(noDoubleNegatives, stmt)

def flatlen(x):
    if type(x) is not types.ListType:
        return 1
    n = 0
    for y in x:
        n += flatlen(y)
    return n

def findPossibleInterpretations(antecedent):
    if type(antecedent) is not types.ListType:
        if antecedent in statements:
            return [ PatternMatch() ]
        else:
            return [ ]
    lst = [ ]
    if antecedent[0] is Or:
        for x in antecedent[1:]:
            lst += findPossibleInterpretations(x)
        return lst
    elif antecedent[0] is And:
        def findAndInterpretations(terms):
            interps = [ PatternMatch() ]
            for t in terms:
                newInterps = [ ]
                for I in interps:
                    # try to find an existing statement that
                    # matches t, given pre-existing context in I
                    for operand in statements:
                        try:
                            I.match(t, operand)
                            newInterps.append(I)
                        except:
                            # exception thrown due to mismatch
                            pass
                interps = newInterps
                if not interps:
                    break
            return interps
        lst += findAndInterpretations(antecedent[1:])
        # a hard problem, interpretations must be consistent
        # first get all interpretations for antecedent[1], then
        # for each one, find all consistent interpretations for
        # antecedent[2], then for each of those, find consistent
        # interpretations for antecedent[3], and so on
        return lst
    for operand in statements:
        try:
            m = PatternMatch(antecedent, operand)
            lst.append(m)
        except:
            # exception thrown due to mismatch
            pass
    return lst

#################################################################

Man = Symbol("man")
Woman = Symbol("woman")
Human = Symbol("human")
Vampire = Symbol("vampire")
Mortal = Symbol("mortal")

# general statements
statements = [
    # All humans are mortal
    [Implies, [IsA, X, Human], [IsA, X, Mortal]],
    # Anybody who is a man or a woman is a human
    [Implies,
     [Or, [IsA, X, Man], [IsA, X, Woman]],
     [IsA, X, Human]],
    ]

#################################################################

if "-d" in sys.argv[1:]:
    debugFlag = True

if "-a" in sys.argv[1:]:
    allFlag = True

Socrates = Symbol("Socrates")
Dracula = Symbol("Dracula")

# specific facts
statements += [
    [IsA, Socrates, Man],
    [Not, [IsA, Dracula, Mortal]],
    ]

if "-c" in sys.argv[1:]:
    # test a contradiction
    statements += [
        [IsA, Dracula, Man]
        ]

def addNew(stmt):
    global statements
    gotIt = False
    for oldStmt in statements:
        if equivalent(oldStmt, stmt):
            gotIt = True
            break
    if not gotIt:
        class Contradiction(Exception):
            pass
        def contradiction(stmt):
            def check(stmtA, stmtB):
                if type(stmtA) is not types.ListType:
                    return False
                if len(stmtA) < 2:
                    return False
                if stmtA[0] is not Not:
                    return False
                return equivalent(stmtA[1], stmtB)
            for oldStmt in statements:
                if check(stmt, oldStmt) or check(oldStmt, stmt):
                    return True
            return False
        if contradiction(stmt):
            # Contradictions can be used for reductio-ad-absurdum
            # arguments if I get clever enough. The idea would be
            # that you introduce a new statement, do all the
            # appropriate deductions, and if an exception is raised
            # then the statement is presumed to be false. Or at least
            # incompatible with the previously existing statements.
            #
            # From a human perspective this is a hack. The program
            # has no real understanding of negation; without this
            # clause it could easily accept two contradictory premises
            # as simultaneously true.
            raise Contradiction(stmt)
        dbgprint("new", stmt)
        statements.append(stmt)

while True:
    n = len(statements)
    # generate any new statements you can
    for rule in principles_of_logic + statements:
        if type(rule) is types.ListType and \
                rule[0] is Implies:
            dbgprint("rule", rule)
            antecedent = rule[1]
            consequent = rule[2]
            for m in findPossibleInterpretations(antecedent):
                dbgprint("interpretation", m.map)
                newStmt = m.subst(consequent)
                newStmt = noDoubleNegatives(newStmt)
                addNew(newStmt)
                if newStmt[0] is And:
                    for x in newStmt[1:]:
                        addNew(x)
    if n == len(statements):
        break

def has_variable(stmt):
    if type(stmt) is not types.ListType:
        return stmt.isVariable()
    for x in stmt:
        if has_variable(x):
            return True
    return False

for s in statements:
    if allFlag or not has_variable(s):
        print s
