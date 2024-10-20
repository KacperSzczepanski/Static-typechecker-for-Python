def head(list):
    return list[0]

def tail(list):
    return list[1:]

def sumList(list):
    if list.length() == 0:
        return 0
    else:
        return head(list) + sumList(tail(list))

def concat(list):
    if list.length() == 0:
        return []
    else:
        return head(list) + concat(tail(list))

suma = sumList([1, 2, 3, 4])
suma2 = concat([[1, 2], [3, 4]]) #length cant be computed in static analysis, so [] is possible result