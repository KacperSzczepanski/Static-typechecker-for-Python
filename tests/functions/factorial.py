def factorial(n):
    if n <= 1:
        return 1
    else:
        return n * factorial(n - 1)

a = factorial(5)
b = factorial(5.5) #result of comparision cant be computed in static analysis, so 1 is possible result