from numpy import * 
from random import * 

height = 20
width = 75
world=ndarray(shape=(height,width), dtype=integer)
gl=int(floor(height/2)) # Ground level

# Block types
stone=5
air=0
grass=1
wood=3
leaves=7   
dirt=4

def mcpy():
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
        world[i+1][j]=dirt
        world[i+2][j]=dirt
        i=randint(i-1,i+1)
        if i > gl+1:
            i=gl
        if i < gl-2:
            i=gl-1

    # fixing the underground/hollow grass
    # indicator toggle pattern
    # note that here the scan order is reversed j around i.
    for j in range(0,width):    
        ind=False
        for i in range(0,height):
            if grass==world[i][j]:
                ind=True
            else:
                if ind==False:
                    if world[i][j]!=air:
                        world[i][j]=air
                else: #ind==true
                    if world[i][j]==air:
                        world[i][j]=stone

    # adds trees
    c=0
    for j in range(3,width-3):
        for i in range(0,height):
            if grass==world[i][j]:
                if randint(0,4)==0 and c<0: 
                    tree(i,j)
                    c=2
                else:
                    c=c-1

    # See what we've got!
    print_world() 

def tree(i,j):
    world[i-1][j]=wood
    world[i-2][j]=wood
    world[i-3][j-2]=leaves
    world[i-3][j-1]=leaves
    world[i-3][j]=wood
    world[i-3][j+1]=leaves
    world[i-3][j+2]=leaves
    world[i-4][j-1]=leaves
    world[i-4][j]=leaves
    world[i-4][j+1]=leaves
    world[i-5][j]=leaves

# Displays a world
chars=" -~!@#$%^&*()_+"
def print_world():
    for i in range(0,height):
        for j in range(0,width):
            print chars[world[i][j]],
        print

#mcpy()

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

#experimenting with curses


# Draw text to center of screen
import curses

# Make a function to print a line in the center of screen
def print_center(message):
    screen = curses.initscr()
    curses.noecho()
    num_rows, num_cols = screen.getmaxyx()
    # Calculate center row
    middle_row = int(num_rows / 2)
    # Calculate center column, and then adjust starting position based
    # on the length of the message
    half_length_of_message = int(len(message) / 2)
    middle_column = int(num_cols / 2)
    x_position = middle_column - half_length_of_message
    # Draw the text
    screen.addstr(middle_row, x_position, message)
    screen.refresh()
    # Now go into a loop that read keystrokes and display the characters typed in
    # a random location on the screen, until 'z' is typed, at which point you stop
    while True:
        v=screen.getch()
        screen.addch(randint(1,num_rows-1),randint(1,num_cols-1),v)
        if v==ord('z'):
            curses.endwin()
            break

print_center("Hello from the center!")

