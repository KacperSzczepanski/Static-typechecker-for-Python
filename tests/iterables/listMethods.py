#I am aware of .length() not being a thing in Python lists
#but it seemed like a perfect opportunity to showcase the most basic usage of methods

def sumList(list):
    if list.length() == 0:
        return 0
    else:
        return list[0] + sumList(list[1:])

x = sumList([1, 2, 3, 4])
y = sumList([1.0, 2.0, 3.0, 4.0]) #length cant be computed in static analysis, so 0 is possible result 
l1 = ['a', 'b', 'c'].append('d')
l2 = []
l3 = l2.append(5)