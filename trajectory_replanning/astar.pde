Maze maze1, maze2;
PathFinder path1, path2;

int SCALE = 20;
int index = 0;

void setup() {
//    println("setup");
    
    reload();

    SCALE = (int) (600 / maze1.size);
	
    size(maze1.size * SCALE, maze1.size * SCALE);// + SCALE * 3 + 1);    
    background(255);

}

void draw() {
    background(255);
    stroke(0);
    line(0, SCALE * maze1.size, width, SCALE * maze1.size);
//    line(SCALE * maze1.size, 0, SCALE * maze1.size, height);
    drawPath(path1, maze1);
//    translate(SCALE * maze1.size + 1, 0);
//    drawPath(path2, maze2);
}



void reload() {
  frameRate(1000);

//    println("loading maze "+(index + 1));
//    String[] content = loadStrings("bad_maze.dat");
//    String[] content = loadStrings("tiny_maze.dat");
    String[] content = loadStrings("/trajectory_replanning/maze"+(index + 1)+".dat");
//	{"5", "     ", "     ", "  X  ", "  X  ", "  AXT"};
    maze1 = new Maze(content);
    int rand = 0; //int(random(3));
    if (rand == 0) {
//	println("backwards");
	path1 = new RepeatedBackwardPathFinder(maze1);
    } else if (rand == 1) {
//	println("adaptive");
	path1 = new AdaptivePathFinder(maze1);
    } else {
//	println("regular");
	path1 = new PathFinder(maze1);
    }
//    println("loaded");

    console.log(maze1);
    window.maze = maze1;

    index = (index + 1) % 30;
}

void keyReleased() {
    if (!path1.step()) {
	println("no solution");
    }
}

void drawPath(PathFinder path, Maze maze) {
    noStroke();
    
    for (int x = 0; x < maze.size; ++x) {
//    println("drawing @ " +x);
	for (int y = 0; y < maze.size; ++y) {
//    println("drawing @ " +y);

	    if (maze.isBlocked(x,y)) {
              drawCell(x, y, 0);
            } else if (maze.cell(x,y) == maze.UNKNOWN) {
              drawCell(x, y, color(0, 0, 5*maze.state(x, y).h(), 120 + 5 * (maze.hiddenIsBlocked(x, y) ? 1 : 0)));
            } else {
              drawCell(x, y, color(0,0,255, maze.state(x,y).f()));
	    }
	}
    }    
    
    Iterator iter;
    State state;

    // open list
    iter = path.search.open.iterator();
    while (iter.hasNext()) {
	state = (State) iter.next();
	drawCell(state.x, state.y, color(255, 0, 0, 128));
    }

    // closed list
    iter = path.search.closed.iterator();
    while (iter.hasNext()) {
	state = (State) iter.next();
	drawCell(state.x, state.y, color(255, 128, 0, 128));
    }

    if (path.reached()) {
//	println(path.expanded + " nodes expanded");
//	println(path.actualPathLength() + " steps");
	iter = path.actualPath();	
	reload();
//	frameRate(0.5);
    } else {
	iter = path.plannedPath();
    }

    // path
    while (iter.hasNext()) {
	state = (State) iter.next();
	drawCell(state.x, state.y, color(255, 255, 0, 255));
    }

    // current location
    drawCell(path.location().x, path.location().y, color(255, 0, 255));

    state = maze.goalState();
    drawCell(state.x, state.y, color(0, 0, 255));
    state = maze.startState();
    drawCell(state.x, state.y, color(0, 255, 0));

  if (!path.step()) {
//	println("no solution");
	reload();
	return;
  }

/*
    fill(0, 255, 0);
    rect(0, maze.size * SCALE + 1, path.search.open.size()/10, SCALE);
    fill(255, 255, 0);
    rect(0, maze.size * SCALE + SCALE + 1, path.search.closed.size()/100, SCALE);
    fill(0, 0, 255);
    rect(0, maze.size * SCALE + 2 * SCALE + 1, path.searches, SCALE);
*/
}

void drawCell(int x, int y, color colour) {
    fill(colour);
    rect(x*SCALE, y*SCALE, SCALE, SCALE);
}
