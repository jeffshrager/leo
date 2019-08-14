# To do:

#   Allow player to place blocks
#   Add player inventory
#   Add mobs
#  Day-night cycle
#  Use pygame for textures
#  water,lava,sand,glass,wood planks
#  main menu, with 'new world' 'load world', and'options'
# ability to save worlds
# 'survival' mode(no flight, slower speed, health, limited resourses)
#  Add biomes
#  crafting
#   Refactor trees so there can be variety
# render distance
# multiplayer?
import time
import curses
from numpy import * 
from random import * 
from threading import Timer

global height, width, world, gl, screen, player, survival

class person:
  def __init__(self, row, col):
    self.row = row
    self.col = col
survival=False
chars=" -~!@#$%^&*()_+="
# Block types (do not use 10)
air=0
grass=1
iron=2
wood=3
dirt=4
stone=5
gold=6
leaves=7   
sand=8
coal=9
diamond=11
woodplank=12
slab=13
cobblestone=14
water=15
def mcpy():
    global height, width, world, gl, screen, player
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
                if randint(1,25)==1 and randint(1,19)<11: 
                    world[row][col]=coal
                if randint(1,45)==1 and randint(1,19)<7:
                    world[row][col]=iron
                if randint(1,80)==1 and randint(1,19)<4 and row>30:
                    world[row][col]=gold
                if randint(1,100)==1 and randint(1,19)<2 and row>20:
                     world[row][col]=diamond

    # Player 
    col=int(floor(width/2))
    for row in range(0,height-1):
        if air!=world[row][col]:
            player=person(row-1,col)
            break
    # See what we've got!
    print_world() 

def tree(row,col): 
    global height, width, world, gl, screen, player
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
def print_world():
    global height, width, world, gl, screen, player
    # For reasons we don't completely understand this can't go to the full height
    # Seems like curses can't display in either the bottom row
    for row in range(0,height-1): 
        for col in range(0,width):
            screen.addch(row,col,chars[world[row][col]])
    screen.addstr(0, 0, "Pycraft Alpha 1.4.0_01")
    screen.addch(player.row,player.col,'*')

def fall():
  moveplayer(player.row+1,player.col)
  moveplayer(player.row+2,player.col)

def mcpy_curses():
    global height, width, world, gl, screen, player, survival
    screen = curses.initscr()
    curses.curs_set(0) 
    screen.keypad(1) 
    curses.mousemask(1)
    curses.noecho()
    height, width = screen.getmaxyx()
    world=ndarray(shape=(height,width), dtype=integer)
    # Calculate center row
    gl = int(height / 2)
    mcpy()
    holding=air # Default placing to air (== breaking???)
    while True:
        v=screen.getch()
        if survival: 
          t = Timer(0.1, fall)
          t.start() 
        if v==ord('l'):
            curses.endwin()
            break
        if v==ord('n'):
            screen.refresh()
            mcpy()
        if v==ord('m'):
          if survival:
            survival = False
            screen.addch(0, width-1,'C')
          else:
            survival=True
            screen.addch(0, width-1,'S')
        if v==ord('j'):
          survival=False
        if v==ord('w'):
          moveplayer(player.row-1,player.col)
        if v==ord('a'):
            moveplayer(player.row,player.col-1)
        if v==ord('d'):
            moveplayer(player.row,player.col+1)
        if v==ord('s'):
            moveplayer(player.row+1,player.col)
        if v==ord('e'):
          holding=(holding+1)%15
          screen.addch(0,width-2,chars[holding])
        if v == curses.KEY_MOUSE:
          _, col, row, _, _ = curses.getmouse()
          d=sqrt(((row-player.row)**2)+((col-player.col)**2))
          if d<5 and d>0.5: # 0.5 keeps you from erasing player
            screen.addch(row, col,chars[holding])
            world[row][col]=holding
    curses.endwin() # Give us normal window control back
    

def moveplayer(nrow,ncol):
    global height, width, world, gl, screen, player
    if nrow>height-2 or nrow<2 or ncol>width-1 or ncol<0 or world[nrow][ncol]!=air:
        True
    else:
        screen.addch(player.row,player.col,' ')
        player.row=nrow
        player.col=ncol
        screen.addch(player.row,player.col,'*')
        screen.addch(0,0,'P')

    

mcpy_curses() # Call the main function
curses.endwin() # Give us normal window control back


