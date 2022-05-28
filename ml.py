# The world is only 10x3. The player starts a the left end at the
# bottom (0,0). (Note that the world is confusingly upside down
# because array coordinates are upside down from world coordinates!)
# Every third block at the bottom is "lava" and has to be
# jumped. Jumping always goes up to the top (2) of the p+1 position,
# and then you descend straight down, unless you go forward or jump
# again. So a jump is like a right move and a +2 up. Mobs show up
# every second tick and move one position to the left on each tick.

from random import *
from sys import *

# Our coords are [left-to-right position, height level]
#  0   1   2   3   4   5   6   7   8   9
# [ ] [ ] [ ] [ ] [ ] [ ] [ ] [ ] [ ] [ ] 2
# [ ] [ ] [ ] [ ] [ ] [ ] [ ] [ ] [ ] [X] 1
# [S] [ ] [L] [ ] [ ] [L] [ ] [ ] [L] [ ] 0 <- Mobs come form here every third tick

lava = [[2,0],[5,0],[8,0]]

# Moves is a list of keywords from {'r', 's', 'j'} right, forward, jump
# (Recall that jump is also a rightward move!)

def play(moves):
    player=[0,0] # Start the player at the left bottom
    exit=[9,2] # The exit is at the right end, half way up the wall
    mobs=[]
    for m in range(0,12):
        #print(mobs)
        # Every three ticks we launch a mob
        if (m % 3) == 0:
            mobs=mobs+[[10,0]] # Mobs start here
        # Hit anything?
        if player==exit:
            #print(moves)
            #print("Found the exit! SUCCESS!")
            return([True,m]) # return the program counter so learner knows min length of winning program
        elif player in mobs:
            #print("Hit by a mob! FAIL!!")
            return([False,player[0]])
        elif player in lava:
            #print("Stepped in Lava! FAIL!!")
            return([False,player[0]])
        # Go ahead and move:
        if moves==[]:
            print("Ran out of program! FAIL!!")
            return([False,player[0]])
        move = moves[m]
        if move == 'r':
            if player[0] < 9:
                player[0]=player[0]+1
        elif move == 'j' and player[1]<2:
            if player[1]==0:
                player[1]=player[1]+2
            elif player[1]==1:
                player[1]=player[1]+1
            # If jump and not at end, also move right
            if player[0]<9: 
                player[0]=player[0]+1
        elif move == 's':
            pass
        elif move == 'j':
            pass
        else:
            print("Bad command: "+move)
            return([False,-999])
        # Gravity falls:
        if move != 'j' and player[1]>0:
            player[1]=player[1]-1
        # Report
        #print(str(move)+'->'+(str(player)))
        # Finally, move all the mobs to the left by one
        for mob in mobs:
            mob[0]=mob[0]-1
    #print("Out of time!")
    return([False,-888])

def watch(moves):

good_programs = []

# Try to find programs randomly (At the moment there are ~3 god programs/1000)
def randomize(n=1000):
    global good_programs
    good_programs = []
    for p in range(1,n+1):
        program = []
        for s in range(0,12):
            program=program+[choice(['r', 'j', 's'])]
        result = play(program)
        if result[0]:
            good_programs = good_programs + [program]
    for gp in good_programs:
        print(gp)

# Enumerate all 12-long programs

def enumprogs():
    global good_programs
    good_programs = []
    successes = 0
    program = ['x' for i in range(0,12)]
    for p in range(0,3**13-1):
        if (p % 100000) == 0:
            print(p,successes, program)
        strb = '{:012}'.format(int(baseN(p,3)))
        for sp in range(0,12):
            program[sp]='rjs'[int(strb[sp])]
        if play(program)[0]:
            successes=successes+1
            good_programs = good_programs + [program]
            program = ['x' for i in range(0,12)]
    print(successes)
    for gp in good_programs:
        print(gp)

def baseN(num,b,numerals="0123456789abcdefghijklmnopqrstuvwxyz"):
    return ((num == 0) and numerals[0]) or (baseN(num // b, b, numerals).lstrip(numerals[0]) + numerals[num % b])    

randomize(1000)    
#enumprogs()

