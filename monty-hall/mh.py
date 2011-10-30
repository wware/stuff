from random import choice

TRIALS = 100000

n = 0
for i in range(TRIALS):
    door_with_car = choice([1,2,3])

    # I choose door #1

    # Monty Hall chooses 2 or 3, but never the door with the car behind it
    lst = [2, 3]
    try:
        lst.remove(door_with_car)
    except ValueError:
        pass

    mh_opens = choice(lst)

    if 1 == door_with_car:   # probability that my choice has the car?
        n += 1

print (1. * n) / TRIALS
