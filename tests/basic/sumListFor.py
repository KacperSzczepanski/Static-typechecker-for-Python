def sumList(list):
    result = 0
    for i in list:
        result = result + i
    return i

def concatList(list):
    result = []
    for i in list:
        result = result + i
    return i

l1 = [1, 2, 3, 4]
l2 = [[1], [2], [3, 4]]
a = sumList(l1)
b = concatList(l2)