# To do:
#   Add caves
#   Add simple player (*) and allow to move around
#   Allow player to break blocks and place blocks
#   Add player inventory
#   Add mobs
#  Day-night cycle
#  Use pygame for textures
#  water,lava,sand,glass,wood planks
#  main menu, with 'new world' 'load world', and'options'
# ability to save worlds
#  Add biomes
# 'survival' mode
#  crafting
#   Refactor trees so there can be variety
# render distance
# multiplayer?

import curses
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
    # adds caves
    for col in range(3,width-3):
        for row in range(3,height-3):
            if stone==world[row][col]:
                if randint(0,60)==0: 
                    cave(row,col)
                    

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

def cave(row,col):
    world[row][col]=air
    world[row-1][col]=air
    world[row-1][col-1]=air
    world[row][col-1]=air
    world[row+1][col]=air
    world[row+1][col+1]=air
    world[row][col+1]=air
    world[row][col]=air
    world[row][col]=air

# Displays a world
chars=" -~!@#$%^&*()_+"
def print_world():
    global height, width, world, gl, screen
    # For reasons we don't completely understand this can't go to the full height
    # Seems like curses can't display in either the bottom row
    for row in range(0,height-1): 
        for col in range(0,width):
            screen.addch(row,col,chars[world[row][col]])

def mcpy_curses():
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
        if v==ord('q'):
            curses.endwin()
            break
        if v==ord('r'):
            screen.refresh()
            mcpy()
    curses.endwin() # Give us normal window control back
    
mcpy_curses() # Call the main function
curses.endwin() # Give us normal window control back
