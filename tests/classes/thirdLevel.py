class Point():
    def __init__(self, x, y):
        self.x = x - 0
        self.y = y - 0
    
    def __add__(p1, p2):
        return Point(p1.x + p2.x, p1.y + p2.y)

    def areaOfSquare(self):
        return self.x * self.y
    
class Point2():
    def __init__(self, p):
        self.x = p.x - 0
        self.y = p.y - 0

    def __add__(p1, p2):
        return Point(p1.x + p2.x, p1.y + p2.y)
    
    def distance2(self, p):
        return (self.x - p.x) ** 2 + (self.y - p.y) ** 2

a = Point(2j, 2) #Point (complex, int)
b = Point(3, 5.5) #Point (int, float)
c = a + b #Point (complex, float)
d = Point2(c) #Point2 (Point (complex, float))
e = Point2(a + a) #Point2 (Point (complex, int))
f = d + e #Point (complex, float)

g = b.areaOfSquare() #float
h = f.areaOfSquare() #complex
i = e.distance2(e) #complex
j = Point2(Point(1, 2)).distance2(Point(3, 4)) #int