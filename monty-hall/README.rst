The Monty Hall Problem
======================

One day you appear as a contestant on a game show, and the host shows
you three closed doors. He tells you that there is a wonderful prize
behind one of the three doors, and he explains this procedure.

* You choose a door.
* He opens one of the other two doors, revealing that it does not have
  the wonderful prize behind it. He can be sure of this because he knows
  which door has the prize.
* You either keep the door you chose, or you choose the remaining third
  door. If you choose correctly, you get the wonderful prize.

The trick is to maximize your probability of getting the wonderful prize.
The file **mh.py** is a simulation that samples scenarios and gives an
approximation for the probability of getting the correct door.

You might imagine that once the host opens one of the doors, the two
remaining doors each have a 1/2 probability of having the prize. The
simulation shows that in fact your original choice has only 1/3
probability of having the prize, and you should switch to the other
door whose probability is now 2/3.

Why is it not 1/2? This took me a little while to understand.

Initially any door has a 1/3 probability of having the prize. You
choose a door, and your choice has a probability of 1/3. So there is
one case where you chose the correct door, and two cases where you
chose the wrong door.

When the host opens a door, there is a 2/3 probability that his hand was
forced -- he could only open THAT door because the remaining door had the
prize -- and a 1/3 probability that he could choose freely between the two
doors you didn't choose. If his choice was forced, you should switch because
that tells you where the prize is. If he was free to choose then you should
keep your original choice.

Another way to say this is that your original probability of 1/3 did not
change by the opening of a different door.
