# infix parser for use with code grepper
# See http://en.wikipedia.org/wiki/Shunting-yard_algorithm
#
# I want to be able to specify simple logical combinations like
# hunt Foo and ( Bar or not Quux )
# The output of the parser is an RPN-order list of operators and operands

# I don't know if I want to use symbols or words for this.
# Still thinking about it.
And = "&"
Or = "|"
Not = "!"

# intern these strings so they can be tested with "is"
def precedence(op1, op2):
    # return -1, 0, or 1, just like strcmp
    precedences = {
        Not: 3,
        And: 2,
        Or: 1,
        "(": 0
        }
    return cmp(precedences[op1], precedences[op2])

def leftAssociative(op):
    return op in (And, Or)

def rightAssociative(op):
    return False

# how do we handle monadic operator Not ??
# apparently Not just works, I got lucky.

def parse(src):
    outputQueue = [ ]
    stack = [ ]
    for token in src:
        if token == "(":
            stack.append(token)
        elif token == ")":
            while stack[-1] != "(":
                outputQueue.append(stack.pop())
            stack.pop()  # discard LParen
        elif token in (And, Or, Not):
            while (len(stack) > 0 and
                   ((precedence(token, stack[-1]) <= 0 and
                     leftAssociative(token)) or
                    (precedence(token, stack[-1]) < 0 and
                     rightAssociative(token)))):
                outputQueue.append(stack.pop())
            stack.append(token)
        else:
            outputQueue.append(token)
    while len(stack) > 0:
        outputQueue.append(stack.pop())
    # output is in RPN order
    return outputQueue

A, B, C, D = "alpha", "bravo", "charlie", "delta"

if __name__ == "__main__":
    lst = "! alpha".split()
    assert parse(lst) == [A, Not]
    lst = [Not, A]
    assert parse(lst) == [A, Not]
    lst = [A, And, B]
    assert parse(lst) == [A, B, And]
    lst = [Not, A, And, B]
    assert parse(lst) == [A, Not, B, And]
    lst = [A, And, Not, B]
    assert parse(lst) == [A, B, Not, And]
    lst = [A, And, "(", B, Or, C, ")"]
    assert parse(lst) == [A, B, C, Or, And]
    lst = [A, And, "(", "(", B, Or, C, ")", ")"]
    assert parse(lst) == [A, B, C, Or, And]
    lst = [A, And, "(", "(", "(", B, Or, C, ")", ")", ")"]
    assert parse(lst) == [A, B, C, Or, And]
    lst = ["(", A, And, "(", B, Or, C, ")", ")"]
    assert parse(lst) == [A, B, C, Or, And]
    lst = [A, And, B, Or, C, And, D]
    assert parse(lst) == [A, B, And, C, D, And, Or]
    lst = [A, Or, B, And, C]
    assert parse(lst) == [A, B, C, And, Or]
    lst = ["(", A, Or, B, ")", And, C]
    assert parse(lst) == [A, B, Or, C, And]
    lst = [Not, A, Or, C, And, Not, B]
    assert parse(lst) == [A, Not, C, B, Not, And, Or]
    print "Tests passed"

# we're not there yet. I need an efficient implementation of
# A-and-not-B search, or what you might call A-minus-B search,
# where first you find all the matches for A, then you grep them
# for B and discard those that grep.

