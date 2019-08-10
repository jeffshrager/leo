from numpy import * 
from random import * 

global height, width, world, gl, screen

height = 20
width = 75
world = False # Gets set in run_mcpy by curses
gl=int(floor(height/2)) # Ground level
screen = False # Gets set in run_mcpy by curses

# Block types
stone=5
air=0
grass=1
wood=3
leaves=7   
dirt=4

def mcpy():
    global height, width, world, gl, screen
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
        mtrange = int(height/10)
        if i > gl+mtrange:
            i=i-1
        if i < gl-mtrange:
            i=i+1

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
    global height, width, world, gl, screen
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
    global height, width, world, gl, screen
    for i in range(0,height-1):
        for j in range(0,width-1):
            screen.addch(i,j,chars[world[i][j]])

# Draw text to center of screen
import curses

# Make a function to print a line in the center of screen
def run_mcpy():
    global height, width, world, gl, screen
    screen = curses.initscr()
    curses.noecho()
    height, width = screen.getmaxyx()
    world=ndarray(shape=(height,width), dtype=integer)
    # Calculate center row
    gl = int(height / 2)
    screen.refresh()
    mcpy()
    while True:
        v=screen.getch()
        if v==ord('z'):
            curses.endwin()
            break
    curses.endwin()
    
run_mcpy()
curses.endwin()
