a = 5
b = 5.0
c = 5j
d = True

w = a and b # [int, float]
x = c and d # [complex, bool]
y = w and x # [int, float, complex, bool]
z = w and c # [int, float, complex]
