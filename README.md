# wumpus

An expanded, rich take on the traditional Wumpus world features in many an AI book.
The worlds in this project are possibly unbounded 2D grids with aggressive Wumpuses and agents roaming the
landscape in search of food, gold, and each other. An agent in a Wumpus world has to survive against hostile entities
and periodically satisfy its hunger. To accomplish that, it may either collect fruits from continuously replenishing
plants or slay Wumpuses or other agents for meat. Alternatively, it can collect the gold strewn around the world in
the hope of trading it for food.

The simulation is only a framework, and agent-AIs plugins therein. In each round, an agent received a limited amount
perceptions from the world simulation and has to return the action it wishes to take based on them and its arbitrary
internal state. It's not only possible to place agents with different parameters, but also entirely different kinds
of agents into the same world and see how they far with or against each other. Of course, an agent never knows
whether a newly encountered stranger is friend or foe, so suspicion is advised.

Features
========

* 2D grid world with many possible actions: move, turn, attack, pick up/drop items, attack, send gesture, gift
* World simulator
* Agent interface that agent AIs can implement
* Actions with type `State -> IO (State, Action)` - implement interactive agents an turn the simulation into a game!
* Statistics

Missing features
================

* Graphical interface. I hope you like reading.
* Bug-freeness. I hope you like error traces.
