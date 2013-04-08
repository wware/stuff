def subtract(x, y):
    return x - y

class Foo:
    def add(self, x, y):
        return x + y

    def factorial(self, n):
        if n < 2:
            return 1
        return n * self.factorial(n - 1)

    @classmethod
    def times(cls, x, y):
        return x * y
