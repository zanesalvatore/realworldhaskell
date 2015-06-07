def splitWith(pred, cs):
    R = []
    L = []

    for c in cs:
        if pred(c):
            L.append(c)
        else:
            if L:
                R.append(L)
            L = []

    return R

if __name__ == "__main__":
    even = lambda x: x % 2 == 0
    print(splitWith(even, [1,2,3,5,6,7,8,10,11]))
