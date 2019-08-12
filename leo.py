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
coal=9
iron=2
gold=6
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

    # adds caves
    for row in range(3,height-3):
        for col in range(3,width-3):
            if stone==world[row][col]:
                if randint(0,55)==0: 
                    cave(row,col,randint(1,6))
    # adds trees
    c=0
    for col in range(3,width-3):
        for row in range(0,height):
            if grass==world[row][col]:
                if randint(0,randint(1,11))==0 and c<0: 
                    tree(row,col)
                    c=4
                else:
                    c=c-1
    # adds ores
    for col in range(3,width-3):
        for row in range(0,height):
            if stone==world[row][col]:
                if randint(1,100)==1 and randint(1,19)<11: 
                    world[row][col]=coal
                if randint(1,100)==1 and randint(1,19)<7:
                    world[row][col]=iron
                if randint(1,100)==1 and randint(1,19)<4:
                    world[row][col]=gold
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

def cave(row,col,age):
    global height, width, world, gl, screen
    if world[row][col]==air:        
            world[row][col+1]=air
            world[row+1][col]=air
            world[row][col-1]=air
            world[row-1][col]=air
            return True
    if row>3 and col>3 and row<height-3 and col<width-3 and randint(1,4)>2:
        if age==0:
            world[row][col]=air
        else:
            world[row][col]=air
            cave(row,col+1,age-1)
            cave(row+1,col+1,age-1)
            cave(row+1,col,age-1)
            cave(row,col-1,age-1)
            cave(row-1,col-1,age-1)
            cave(row-1,col,age-1)
            cave(row-1,col+1,age-1)
            cave(row+1,col-1,age-1)

# Displays a world
chars=" -~!@#$%^&*()_+"
def print_world():
    global height, width, world, gl, screen
    # For reasons we don't completely understand this can't go to the full height
    # Seems like curses can't display in either the bottom row
    for row in range(0,height-1): 
        for col in range(0,width):
            screen.addch(row,col,chars[world[row][col]])
    screen.addstr(0, 0, "Pycraft  Alpha 1.1.1_02")

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
        if v==ord('l'):
            curses.endwin()
            break
        if v==ord('n'):
            screen.refresh()
            mcpy()
    curses.endwin() # Give us normal window control back
    
mcpy_curses() # Call the main function
curses.endwin() # Give us normal window control back
