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
    for row in range(0,height):
        for col in range(0,width):
            world[row][col]=air

    # Generate stone base
    for row in range(gl,height):
        for col in range(0,width):
            world[row][col]=stone

    # add surface variation
    row=gl
    for col in range(0,width):
        world[row][col]=grass
        world[row+1][col]=dirt
        world[row+2][col]=dirt
        row=randint(row-1,row+1)
        mtrange = int(height/10)
        if row > gl+mtrange:
            row=row-1
        if row < gl-mtrange:
            row=row+1

    # fixing the underground/hollow grass
    # indicator toggle pattern
    # note that here the scan order is reversed j around i.
    for col in range(0,width):    
        ind=False
        for row in range(0,height):
            if grass==world[row][col]:
                ind=True
            else:
                if ind==False:
                    if world[row][col]!=air:
                        world[row][col]=air
                else: #ind==true
                    if world[row][col]==air:
                        world[row][col]=stone

    # adds trees
    c=0
    for col in range(3,width-3):
        for row in range(0,height):
            if grass==world[row][col]:
                if randint(0,4)==0 and c<0: 
                    tree(row,col)
                    c=4
                else:
                    c=c-1

    # See what we've got!
    print_world() 

def tree(row,col):
    global height, width, world, gl, screen
    world[row-1][col]=wood
    world[row-2][col]=wood
    world[row-3][col-2]=leaves
    world[row-3][col-1]=leaves
    world[row-3][col]=wood
    world[row-3][col+1]=leaves
    world[row-3][col+2]=leaves
    world[row-4][col-1]=leaves
    world[row-4][col]=leaves
    world[row-4][col+1]=leaves
    world[row-5][col]=leaves

# Displays a world
chars=" -~!@#$%^&*()_+"
def print_world():
    global height, width, world, gl, screen
    for row in range(0,height-1):
        for col in range(0,width-1):
            screen.addch(row,col,chars[world[row][col]])

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
        if v==ord('r'):
            screen.refresh()
            mcpy()
    curses.endwin()
    
run_mcpy()
curses.endwin()
