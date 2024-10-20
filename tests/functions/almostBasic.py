def ints():
    return 44

def e1():
    return ints()

def e2():
    return e3(5.5)

def e3(a):
    return a + e1()

def func(a, b):
    return