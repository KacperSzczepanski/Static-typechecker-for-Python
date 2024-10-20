# [a] -> [a, string]
def func1(a):
    if True:
        return a
    else:
        return "a"

# [a, b] -> [void, func1(int), func4(b)]
def func2(a, b):
    if a:
        return func4(b)
    elif b:
        return func1(42)

# [] -> [func2(complex, float)]
def func3():
    return func2(5j, 3.3)

# [a] -> [func3() + a]
def func4(a):
    return func3() + a

# [] -> [void]
def func5():
    True

def silnia(n):
    if n <= 1:
        return 1
    else:
        return n * silnia(n - 1)

def collatzEven(n):
    if n == 1:
        return 1

    n = n / 2

    if n % 2 == 0:
        return collatzEven(n)
    else:
        return collatzOdd(n)
    
def collatzOdd(n):
    if n == 1:
        return 1

    n = n * 3 + 1

    if n % 2 == 0:
        return collatzEven(n)
    else:
        return collatzOdd(n)