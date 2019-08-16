import pygame
from random import *
from time import *
from curses import *
from numpy import *
from threading import *

pygame.init()
 
SCREENWIDTH=1200
SCREENHEIGHT=1000
blockwidth = 10
blockheight = 10
nblocksheight=int(SCREENHEIGHT/blockheight)
nblockswidth=int(SCREENWIDTH/blockwidth)
world=ndarray(shape=(nblocksheight,nblockswidth), dtype=integer)
gl = int(blockheight / 2)
print(blockwidth, blockheight, nblockswidth, nblocksheight, SCREENWIDTH, SCREENHEIGHT)

size = (SCREENWIDTH, SCREENHEIGHT)
screen = pygame.display.set_mode(size)

font = pygame.font.Font(None, 36)
pygame.font.init()
 
print('Screen and font initialized')

# Colors:

WHITE = (255, 255, 255)
GREEN = (20, 255, 140)
GREY = (210, 210 ,210)
WHITE = (255, 255, 255)
RED = (255, 0, 0)
PURPLE = (255, 0, 255)
YELLOW = (255, 255, 0)
CYAN = (0, 255, 255)
BLUE = (100, 100, 255)

# Object class definitions:

class Block():
    global blockwidth, blockheight, nblockswidth, nblocksheight, SCREENWIDTH, SCREENHEIGHT, world
    #This class represents a car. It derives from the "Sprite" class in Pygame.
 
    def __init__(self, color):
 
        # Pass in the color of the car, and its x and y position, width and height.
        # Set the background color and set it to be transparent
        self.image = pygame.Surface([blockwidth, blockheight])
        self.image.fill(WHITE)
        self.image.set_colorkey(WHITE)
 
        self.color = color
 
        # Draw the car (a rectangle!)
        pygame.draw.rect(self.image, self.color, [0, 0, blockwidth, blockheight])

        self.rect = self.image.get_rect()
 
    def repaint(self, color):
        self.color = color
        pygame.draw.rect(self.image, self.color, [0, 0, blockwidth, blockheight])

class Ent(pygame.sprite.Sprite):
    #This class represents a car. It derives from the "Sprite" class in Pygame.
 
    def __init__(self, color, width, height, speed):
        # Call the parent class (Sprite) constructor
        super().__init__()
 
        # Pass in the color of the car, and its x and y position, width and height.
        # Set the background color and set it to be transparent
        self.image = pygame.Surface([width, height])
        self.image.fill(WHITE)
        self.image.set_colorkey(WHITE)
 
        #Initialise attributes of the car.
        self.width=width
        self.height=height
        self.color = color
        self.speed = speed
 
        # Draw the car (a rectangle!)
        pygame.draw.rect(self.image, self.color, [0, 0, self.width, self.height])
 
        # Instead we could load a proper picture of a car...
        # self.image = pygame.image.load("car.png").convert_alpha()
 
        # Fetch the rectangle object that has the dimensions of the image.
        self.rect = self.image.get_rect()
 
# Game things:

start=False
scoreIncrementTimer = 0
ticksSinceLastFrame = pygame.time.get_ticks()
global score
score = 0

# Blocks:

blockcolors=[GREEN,GREY,WHITE,RED,PURPLE,YELLOW,CYAN,BLUE,GREEN,GREY,WHITE,RED,PURPLE,YELLOW,CYAN,BLUE]
 
speed = 1
colorList = (RED, GREEN, PURPLE, YELLOW, CYAN, BLUE)
 
#This will be a list that will contain all the sprites we intend to use in our game.
all_sprites_list = pygame.sprite.Group()

''' 
player2 = Ent(CYAN, 60, 80, 70)
player2.rect.x = 160
player2.rect.y = 160
 
zombie = Ent(BLUE, 60, 80, random.randint(50,100))
zombie.rect.x = 360
zombie.rect.y = -900
 
# Add the car to the list of objects
all_sprites_list.add(player2)
all_sprites_list.add(zombie)
 
all_coming_cars = pygame.sprite.Group()
all_coming_cars.add(zombie)
''' 

#Allowing the user to close the window...
carryOn = True
clock=pygame.time.Clock()
start=True 
lastFrameTicks = 0

# ==================== Old PyCraft code ===================

class person:
  def __init__(self, row, col):
    self.row = row
    self.col = col
    self.health = 20

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
    '''
    # adds lakes
    for col in range(8,width-8):
      for row in range(0,height):
        if grass==world[row][col]:
          if randint(1,80)==1:
            for i in range(1,randint(1,30)):
              world[randint(row-5,row+5)][randint(col-5,col+5)]=water
              i=i+1
    '''
        
    
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

# Simlified world init

def witest():
    global nblockswidth, nblocksheight, blockheight, blockwidth, world, gl, screen, player
    for row in range(0,nblocksheight):
        for col in range(0,nblockswidth):
            world[row][col]=air
    for row in range(gl,nblocksheight):
        for col in range(0,nblockswidth):
            world[row][col]=stone
    row=gl
    for col in range(0,nblockswidth):
        world[row][col]=grass
        world[row+1][col]=dirt
        world[row+2][col]=dirt
        row=randint(row-1,row+1)
        mtrange = int(nblocksheight/10)
        if row > gl+mtrange:
            row=row-1
        if row < gl-mtrange:
            row=row+1

# Displays a world
def pwtest():
    global nblocksheight, nblockswidth, world, gl, screen, player
    # For reasons we don't completely understand this can't go to the full nblocksheight
    # Seems like curses can't display in either the bottom row
    for row in range(0,nblocksheight-1): 
        for col in range(0,nblockswidth):
          block = Block(blockcolors[world[row][col]]) 
          block.rect.x = row
          block.rect.y = col

class perpetualTimer():

   def __init__(self,t,hFunction):
      self.t=t
      self.hFunction = hFunction
      self.thread = Timer(self.t,self.handle_function)

   def handle_function(self):
      self.hFunction()
      self.thread = Timer(self.t,self.handle_function)
      self.thread.start()

   def start(self):
      self.thread.start()

   def cancel(self):
      self.thread.cancel()

def fall():
  moveplayer(player.row+1,player.col)
  moveplayer(player.row+2,player.col)
  screen.addstr(0,20+int(player.health),'*')
  player.health=player.health-1
      
def do_per_keystroke_tasks():
  flow_water()

def flow_water():
  # Scan the whole world for water:
  for row in range(0,height):
    for col in range(3,width-3):
      # When you find a water elt...
      if world[row][col]==water:
        # Check below, and flow down if it's open
        if world[row+1][col]==air:
          flowto(row,col,row+1,col)
        else: 
            # Prioritize either left or right
            if 1==randint(1,2):
              if world[row+1][col-1]==air:
                flowto(row,col,row+1,col-1)
              else:
                if world[row+1][col+1]==air:
                  flowto(row,col,row+1,col+1)
            else:
              if world[row+1][col+1]==air:
                flowto(row,col,row+1,col+1)
              else:
                if world[row+1][col-1]==air:
                  flowto(row,col,row+1,col-1)

def flowto(rowfrom,colfrom,rowto,colto):
  screen.addch(rowto,colto,'=')
  world[rowto][colto]=water
  screen.addch(rowfrom,colfrom,' ')
  world[rowfrom][colfrom]=air

global t

def mcpy_curses():
    global height, width, world, gl, screen, player, survival, t
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
    # Start perpetual fall process for Survival
    t = perpetualTimer(1,fall)
    while True:
        v=screen.getch()
        do_per_keystroke_tasks()
        if v==ord('l'):
            curses.endwin()
            break
        if v==ord('n'):
            screen.refresh()
            mcpy()
        if v==ord('m'):
          if survival:
            survival = False
            t.cancel()
            screen.addch(0, width-1,'C')
          else:
            survival=True
            t.start()
            screen.addch(0, width-1,'S')
        if v==ord('w'):
          moveplayer(player.row-1,player.col)
        if v==ord('a'):
            moveplayer(player.row,player.col-1)
        if v==ord('d'):
            moveplayer(player.row,player.col+1)
        if v==ord('s'):
            moveplayer(player.row+1,player.col)
        if v==ord('e'):
          holding=(holding+1)%16
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
    if (nrow>height-2 or nrow<2 or ncol>width-1 or ncol<0) or not(world[nrow][ncol]==air or world[nrow][ncol]==water):
        True
    else:
        screen.addch(player.row,player.col,chars[world[player.row][player.col]])
        player.row=nrow
        player.col=ncol
        screen.addch(player.row,player.col,'*')
        screen.addch(0,0,'P')

#mcpy_curses() # Call the main function
#curses.endwin() # Give us normal window control back

# ==================== Block game code ===================
witest()
print(world[10][10])
while carryOn:
        for event in pygame.event.get():
            if event.type==pygame.QUIT:
                carryOn=False
            elif event.type==pygame.KEYDOWN:
                if event.key==pygame.K_x:
                     player2.moveRight(10)
 
        keys = pygame.key.get_pressed()
        if keys[pygame.K_LEFT]:
            player2.moveLeft(5)
        if keys[pygame.K_RIGHT]:
            player2.moveRight(5)
        if keys[pygame.K_UP]:
            speed += 0.05
        if keys[pygame.K_DOWN]:
            speed -= 0.05



        '''
        #Game Logic
        for car in all_coming_cars:
            car.moveForward(speed)
            if car.rect.y > SCREENHEIGHT:
                car.changeSpeed(random.randint(50,100))
                car.repaint(random.choice(colorList))
                car.rect.y = -200

        #Check if there is a car collision
        car_collision_list = pygame.sprite.spritecollide(player2,all_coming_cars,False)
        for car in car_collision_list:
            print("Block crash!")
            #End Of Game
            carryOn=False

        thisFrameTicks = pygame.time.get_ticks()
        ticksSinceLastFrame = thisFrameTicks - lastFrameTicks
        lastFrameTicks = thisFrameTicks

        scoreIncrementTimer = scoreIncrementTimer + ticksSinceLastFrame
        if scoreIncrementTimer > 100 and player2.rect.x > 0 and player2.rect.x < 400:
            score = score + (1 * int(speed))
            scoreIncrementTimer = 0
        '''            
        all_sprites_list.update()
        
        pwtest()

        #Now let's draw all the sprites in one go. (For now we only have 1 sprite!)
        all_sprites_list.draw(screen)
        
        #Refresh Screen
        pygame.display.flip()
        
        #Number of frames per secong e.g. 60
        clock.tick(60)
    
pygame.quit()

