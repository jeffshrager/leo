from numpy import * 
from random import * 

def mcpy():
    height = 10
    width = 50
    world=ndarray(shape=(height,width), dtype=integer)
    gl=int(floor(height/2)) # Ground level

    # Block types
    stone=5
    air=0
    grass=1


    # Initialize the whole world to air (0)
    # *** NESTED (2) ITERATOR PATTERN ***
    for i in range(0,height):
        for j in range(0,width):
            world[i][j]=air

    # Generate stone base
    for i in range(gl,height):
        for j in range(0,width):
            world[i][j]=stone

    # add surface variation
    i=gl
    for j in range(0,width):
        world[i][j]=grass
        i=randint(i-1,i+1)
        if i > gl+1:
            i=gl
        if i < gl-2:
            i=gl-1

    # fixing the underground/hollow grass
    # indicator toggle pattern
    # note that here the scan order is reversed j around i.
    for j in range(0,width):    
        ind=0
        for i in range(0,height):
            if grass==world[i][j]:
                ind=1
            else:
                if ind==0:
                    if world[i][j]!=air:
                        world[i][j]=air
                else: #ind==1
                    if world[i][j]!=stone:
                        world[i][j]=stone

    # See what we've got!
    print_world(world) 

# Displays a world
chars=" -~!@#$%^&*()_+"
def print_world(world):
    # Have to get the height and width
    height = size(world,0)
    width = size(world,1)
    for i in range(0,height):
        for j in range(0,width):
            print chars[world[i][j]],
        print

mcpy()

# iterative factorial
def ifact(x):
    r=1 # initialize the result
    # multiply in each num from 1 to x
    for c in range(1,x+1):
        r=c*r
    return r # return the result, whick should be the factorial.

#print(ifact(5))


# recursive factorial
def rfact(x):
    if x==1: # bottom condition: the factorial of 1 is 1!
        return 1
    else: # recursion condition
        return x*rfact(x-1) # the factorial of x is x * the factorial of x-1
    
#print(rfact(5))

btree=['bark?',['dog',None,None],['big?',['tiger',None,None],['cat',None,None]]]
atree=['bark?',['dog',None,None],['big?',["stripes",['tiger',None,None],['elephant',None,None]],['cat',None,None]]]

def ana(a):
    if a[1]==None:
        gotit = raw_input('Is this a ' + a[0] + "?")
        if gotit=='y':
            print('yay!')
        else:
            nana= raw_input('What animal were you thinking of?')
            nq= raw_input('Give me a question for which the answer is YES for a ' + a[0] + ' and NO for a ' + nana)
            a[1]=[a[0],None,None]
            a[2]=[nana,None,None]
            a[0]=nq
    else:
        ans=raw_input(a[0])
        if ans=='y':
            ana(a[1])
        else:
            ana(a[2])
    ana(atree)


#ana(atree)
