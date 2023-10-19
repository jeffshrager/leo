import random

ColorOrder=["Red", "Orange", "Yellow", "Green", "Blue", "Purple"]
RandomColors=[]

# Create a list (1*X Array) with random colors.

for n in range(1,40):
    #TheColor = ColorOrder[random.randint(0, 10)]
    TheColor = random.randint(0, 40)
    RandomColors.append(TheColor)

# Bubble sort the RandomColors based on the position in the ColorOrder
# table. We'll use the fact that if you go through the list and it's
# in the right order, you will never have to do a swap. So we'll set a
# reminder variable whenever we do a swap. If that's not True when
# we're done, then the list is sorted.

print (RandomColors)

NSwaps = 0
AnySwapped = True
while AnySwapped:
    AnySwapped=False
    for p in range(0,38):
        if RandomColors[p]>RandomColors[p+1]:
            # Swap the color at the position with the one following it.
            Hold=RandomColors[p]
            RandomColors[p]=RandomColors[p+1]
            RandomColors[p+1]=Hold
            print "Swapping %d at RandomColors[%d] with %d at RandomColors[%d]" % (RandomColors[p+1],p,RandomColors[p],p+1)
            AnySwapped=True
            NSwaps=NSwaps+1

print (NSwaps)
print (RandomColors)
