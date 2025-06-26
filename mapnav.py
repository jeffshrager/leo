import random
current = "null"
targ = "null"
hp = 10
city_names = [
    "Youngermoore",    # 0
    "Grimehold",     # 1
    "Snuffyreach",     # 2
    "Backthen",     # 3
    "Breadhollow",  # 4
    "Bashinriver",    # 5
    "Yellingwind"   # 6
]
backstack = [] ### This name tells us nothing about its use. 

map_edges = [
    [0, {"mname": "Globlin",         "mhp": -10, "ability": ""}, 1],
    [0, {"mname": "Spider Teen",   "mhp": 20, "ability": "Spider-Teen Web"}, 2],
    [1, {"mname": "Orc Route",      "mhp": 25, "ability": "GPS Error"}, 3],
    [2, {"mname": "Mraith",         "mhp": 70, "ability": "Mrath of the Mraith"}, 3],
    [3, {"mname": "Internet Troll",   "mhp": 4000, "ability": "Nobody Asked"}, 4],
    [2, {"mname": "Gnat Swarm",      "mhp": random.randint(0,100), "ability": "Bite"}, 5],
    [1, {"mname": "Fire Ache",     "mhp": 52, "ability": "COX stimulation"}, 5],
    [4, {"mname": "Copromancer",    "mhp": 150, "ability": "Poop Orb"}, 6],
    [5, {"mname": "Normal Chest",    "mhp": 1, "ability": ""}, 4],
    [0, {"mname": "Skeleton Sam", "mhp": random.randint(100,400), "ability": "Spooky Dance"}, 5],
    [5, {"mname": "Cute Dog",      "mhp": -20}, 6],
    [3, {"mname": "Normal-Sized Toad",     "mhp": 23, "ability": "Rather Deadly Poison"}, 5],
    [1, {"mname": "Smog Sag",        "mhp": 3, "ability": "Big Bag"}, 6],
    [4, {"mname": "Spectator",        "mhp": 0, "ability": ""}, 2],
    [6, {"mname": "Dim Paladin",   "mhp": 260, "ability": "Lights Out"}, 0]
]


def start():
    choosestart = list(range(len(city_names)))
    choosestart.remove(current)
    return random.choice(choosestart)
    ### BTW, this can be done with a single line in various ways -- but that's for later

### Another useful thing to do is to order your functions in a sensible way, usually the callers go right below
### the functions they call (or some people do right above)

def getoutgoing():
    destinations = []
    edges = []
    ### Function comment: takes ... returns ... (although getoutgoing is an ok name, the python standard is to use _ like: get_outgoing)
    for x in range(0,len(map_edges)): ### Giving vars meaningful names (not just x and y) will also help remember what's going on s mcould be map_index, or just index
        if map_edges[x][0] == current:
            destinations.append(map_edges[x][2]) 
            edges.append(map_edges[x]) 
    return destinations,edges

def travel():
    ### Block comment!
    global current
    act,destinations,edge = action() ### take_step()? -- the naming of functions is NOT trivial -- unless you comment everywhere it's the only way you can tell what's going on!
    print("BBBBBBBBBBB act=", act," dest=",destinations," edge=",edge)
    if act == "BACK":
        if len(backstack) > 0: 
            current = pops()
    elif city_names.index(act) in destinations:
        if battle(edge) == 1:
            print("You were victorious.")
            stack()
            current = city_names.index(act)
        elif battle(edge) == 0:
            print("You lost.")
            run()
    else:
        print("Can't do that.")
        travel()
        
def stack(): ### Confusing name -- push_current_on_path_stack()
    global backstack
    global current
    backstack.append(current)
    return stack

def pops(): ### pop_path_stack()
    global backstack
    global current
    q = backstack.pop((len(backstack)-1))
    return q
    
    
def action():
    ### Block comment! (this one's too complex to just have the whole comment be its name)
    act = " "
    destinations,edges = getoutgoing() ### This is where (at least one of) your problem(s) is -- ask me to explain
    print("Your goal is to reach " + str(city_names[targ]) + ".")
    print("You are currently in " + city_names[current] + ". You can go to these cities: ")
    for y in range(0,len(destinations)): ### better var than y?
        workingcity = destinations[y]
        workingmob = edges[y][1]
        print(city_names[workingcity] + ", but you must fight a " + workingmob["mname"] + " with " + str(workingmob["mhp"]) + " health.")
    print("You can also go BACK.")
    act = input("Where would you like to go? ")
    city_number = city_names.index(act)
    print("CCCCCCCCC edges=",edges)
    return act,destinations, [edge for edge in edges if edge[2]==city_number][0]

def battle(mob):
    print("AAAAAAAAAAAAA mob=", mob)
    global hp
    chp = mob["mhp"]
    turn = "P"
    items = ["Big Stick", "Intercontinental Ballistic Missile", "Hyperfixation", "Job Application", "Adult Sword", "Tooth"]
    spells = ["Logical Fallacy", "Poo Tornado", "Invocation of Snuffy", "Super Death Explosion", "Summon " + random.choice(["Air", "Cat", "Death Star", "Worm", "Pizza", "Supermassive Black Hole"]), "Capitalism Blast"]
    print("You are fighting a " + str(mob["mname"]) + " with " + str(mob["mhp"]) + " health.")
    print("You have " + str(hp) + " health.")
    if mob["mhp"] <= 1:
        print("You defeated the " + str(mob["mname"]) + " and regained 2 health.")
        hp = hp + 2
        return 1
    elif chp > 0:
        if turn == "P":
            if hp <= 0:
                print("You died.")
                return 0
            elif chp <= 1:
                print("You defeated the " + str(mob["mname"]) + " and regained 2 health.")
                hp = hp + 2
                return 1
        else:
            move = input("Would you like to cast a SPELL or use your sword to ATTACK? ")
            if move == "SPELL":
                chp == round(chp*0.5)
                print("You cast " + random.choice[spells] + ". The " + mob["mname"] + " has " + str(chp) + " health remaining.")
                turn = "M"
            elif move == "ATTACK":
                chp == chp-random.randint(0,20)
                print("You attacked with your " + random.choice[items] + ". The " + mob["mname"] + " has " + str(chp) + " health remaining.")
                turn =="M"
            else:
                print("Not an option!")
                turn = "M"
        if turn == "M":
            if mob["mname"] == "Spider Teen" or mob["mname"] == "Fire Ache" or mob["mname"] == "Copromancer":
                attack = 2
            elif mob["mname"] == "Skeleton Sam" or mob["mname"] == "Mraith":
                attack = random.randint(1,2)
            elif mob["mname"] == "Orc Route" or mob["mname"] == "Gnat Swarm":
                attack = random.randint(0,2)
            elif mob["mname"] == "Internet Troll":
                attack = random.randint(0,1)
            else:
                attack = 1
            print("The " + mob["mname"] + " used " + mob["ability"] + ", dealing " + str(attack) + " damage." )
            hp = hp - attack
            turn = "P"

def run():
    global current 
    global targ
    current = random.randint(0,6)
    targ = start()
    while current != targ:
        travel()
    print("You were sucessful.")

run()
