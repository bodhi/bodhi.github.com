---
title: Visualisation of Trajectory Replanning
layout: default
---

<del>About a bazillion years ago</del> In 2008, as part of an assignment for my Masters degree, I wrote a visualiser for *Trajectory Replanning* in [Processing](http://processing.org). Here it is, converted to [Processing.js](http://processingjs.org):

<script src="/trajectory_replanning/processing-1.3.6.js"></script>  
<canvas style="margin-top: 40px;" data-processing-sources="/trajectory_replanning/maze.pde /trajectory_replanning/path.pde /trajectory_replanning/search.pde /trajectory_replanning/state.pde /trajectory_replanning/astar.pde">
</canvas>  

<aside markdown="1">
* Maze is in grey, walls are darker.
* Solid black are walls known to the agent.
* Yellow is the agent's plan to get to the goal.
* Pink area is the space considered by the agent when deciding how to get there.
* Solver iterates through multiple mazes with using 3 different algorithms, see below for more scant details.
</aside>

> Trajectory Replanning?!

Yes:

1. I start at the green dot.

2. I want to get to the blue dot.

3. I don't have [perfect
   information](https://en.wikipedia.org/wiki/Perfect_information)
   about the maze. I learn as I explore.

4. I'll use [A\*](https://en.wikipedia.org/wiki/A*_search_algorithm) to
   figure out how to get to the blue dot.

5. Hey, I moved a bit and discovered a wall I didn't know about! My plan is now useless.

So, the assignment was to evaluate 3 different methods for coping with
the change in the agent's *knowledge of the world* that happend at
step 5 above:

1. *Repeated Forward A\**

2. *Repeated Backward A\**

3. *Adaptive A\**

It's late, and the purpose of this was to just get the visualisation
online, so I'm not going to explain the various algorithms. Code is
[on GitHub](https://github.com/bodhi/trajectory), and you can check
out [my assignment report](/trajectory_replanning/report.pdf), if you
like (PDF warning...).