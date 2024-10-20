class A():
    def __init__(self):
        a = 5 and "five"
        self.a = a

    def add(self, x):
        return self.a + x

a = A()
if 1:
    #a is of one specific type, so cannot pass these two calls in the same run
    b = a.add(5)
    c = a.add("five")
elif 2:
    b = a.add(5) + 5.5
else:
    c = a.add("five")