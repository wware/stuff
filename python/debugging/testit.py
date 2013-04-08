import hack
from debugger import investigate, investigate_in

investigate_in(hack)

print hack.subtract(15, 8)

_add = hack.Foo.add

foo = hack.Foo()

print foo.add(3, 4)
print foo.factorial(5)

print hack.Foo.add(foo, 5, 8)

print _add(foo, 5, 8)
