Ant Colony Optimisation on Aircraft Conflict Resolution
-------------------------------------------------------

This project is a quick proof of concept: ant colony optimisation is a valid
algorithm for solving aircraft conflicts.

### The problem

The problem considered is made of several aircraft placed on a circle and
heading towards the centre of this circle. They are all going to conflict before
reaching the centre: they will become too close to each other.

Air traffic controllers are in charge of avoiding this kind of situations; they
can be assisted by software in order to help them reroute the aircraft for a
safe flight configuration.

Aircraft conflict solving problem is a difficult problem, and it can be very
hard to explore all possible combinations, find a route for each aircraft
without conflicts, and minimise the flight delay. Metaheuristics are methods
inspired by nature which, under certain conditions, converge towards an optimum.

Genetic algorithms are commonly used for this problem; however, ant colony
optimisation is a possible alternative.

You can read more about Ant colony optimisation
[here](http://en.wikipedia.org/wiki/Ant_colony_optimization_algorithms) and
about Aircraft conflict resolution on many references pointed by your favourite
search engine.

### History

I wrote the code in OCaml as part of my Master's thesis graduation requirements.
I wrote it under a modern Unix environment with OCaml and relied on the popular
Unix tool ```gnuplot``` for plotting the trajectories as they are computed.

That was 2006.

Today, I did some cleaning in the code in order to publish it. The code
is published as is, with no guarantee whatsoever.

Feel free to copy all that may be of any use to you.

