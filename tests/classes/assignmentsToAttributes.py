class C1():
    def __init__(self, x):
        self.a = 4
        self.b = "five"
        self.x = x

class C2():
    def __init__(self, obj):
        self.x = obj

c1 = C1(3.7) #C1 (float)
c2 = C2(c1) #C2 (C1 (float))

if 1:
    c2.x.a = c2.x.a + 5 #OK
    a = True
elif 2:
    c2.x.a = c2.x.a + 5.0 #NOT OK
    a = 1
elif 3:
    c2.x.b = "six" #OK
    a = 1.0
elif 4:
    c2.x.b = ["seven"] #NOT OK
    a = 1j
elif 5:
    c2.x.x = c2.x.x + 4 #OK
    a = "one"
else:
    c2.x.x = c2.x.x + 4j #NOT OK
    a = [1]