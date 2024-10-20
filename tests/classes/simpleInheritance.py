class C1():
    def __init__(self, a):
        self.a = a - 0

    def __add__(self, o):
        return 44

    def m1(self, a, b):
        return self.a + a + b

class C2(C1):
    def __add__(self, o):
        return o
    
    def m2(self, a):
        return 2 * a
    
class C3(C2):
    def __init__(self, x, y):
        self.x = x - 0
        self.y = y - 0

    def m1(self):
        return self.x

    def m2(self, a):
        return 2j * a
    
    def m3(self):
        return self.y
    
if True:
    x = C3(5, 6) #C3 (int, int)
else:
    x = C2(7) #C2 (int)

y = x.m2(10.1) #complex/float

c1 = C1(1) #C1 (int)
c2 = C2(2) #C2 (int)
c3 = C3(3, 4.5) #C3 (int, float)

a = c1.m1(2j, 3) #4 + 2j
b = c1 + "test" #44
print(a, b)

c = c2.a #2
d = c2 + "test" #"test"
e = c2.m1(2, 3.3) #7.3
f = c2.m2(0) #0
print(c, d, e, f)

g = c3.m1() #3
h = c3.m2(5j) #-10 + 0j
i = c3.m3() #4.5
j = c3 + c1 #C1(1)
print(g, h, i, j)
