# https://projecteuler.net/problem=1

#Multiples of 3 and 5
#Problem 1
#If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
#Find the sum of all the multiples of 3 or 5 below 1000.

# Hint: If you don't remove the 15x's then you'll overcount

>>> (reduce((lambda x, y: x + y),[x*3 for x in range(1,334)]))+(reduce((lambda x, y: x + y),[x*5 for x in range(1,200)]))-(reduce((lambda x, y: x + y),[x*15 for x in range(1,67)]))
233168

# More efficient (maybe?):

def e1():
    s = 0
    for n in range(1,1000):
        if (0 == n%3) or (0 == n%5):
            s += n
    return(s)


>>> e1()
233168

# =========================================================================================================

# Median problem.

import random
from math import trunc

global l,r

def test():
    global l,r
    l=[]
    r=[]
    for i in range(1,100):
        mtest(trunc(100*random.random()))

def mtest(n):
    global l,r
    if []==l:
        if []==r:
            r = [n]+r
        l = [n]+l
    ...
        


    

