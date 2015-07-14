class Maze {
    public int size;
    private int[][] maze;
    private int[][] hiddenMaze;
    private int goalX, goalY, startX, startY;

    private State[][] states;

    public final static int UNKNOWN = -1;
    private final static int EMPTY = 0;
    private final static int WALL = 1;
    private final static int START = 2;
    private final static int GOAL = 3;    

    public Maze(String[] input) {
	size = 1 * input[0];
	maze = new int[size][];
	hiddenMaze = new int[size][];
	states = new State[size][];
	String line;

	for (int i = 0; i < size; ++i) {
	    maze[i] = new int[size];
	    hiddenMaze[i] = new int[size];
	    states[i] = new State[size];

	    line = input[i+1];
	    for (int j = 0; j < size; ++j) {
              states[i][j] = null;

              char c = line.charAt(j);
		if (c == "X") {
		    hiddenMaze[i][j] = WALL;
		    maze[i][j] = UNKNOWN;
		} else if (c == "A") {
		    hiddenMaze[i][j] = START;
		    maze[i][j] = START;
		    startX = j;
		    startY = i;
		} else if (c == "T") {
		    hiddenMaze[i][j] = GOAL;
		    maze[i][j] = GOAL;
		    goalX = j;
		    goalY = i;
		} else {
		    maze[i][j] = UNKNOWN;
		    hiddenMaze[i][j] = EMPTY;
		}		
	    }
	}
    }

    public void fillIn(State state) {
	int x = state.x;
	int y = state.y;

	if (y + 1 < size) maze[y+1][x] = hiddenMaze[y+1][x];
	if (y - 1 >= 0) maze[y-1][x] = hiddenMaze[y-1][x];
	if (x + 1 < size) maze[y][x+1] = hiddenMaze[y][x+1];
	if (x - 1 >= 0) maze[y][x-1] = hiddenMaze[y][x-1];
    }

    public boolean isBlocked(int x, int y) {
	if (x < 0 || x >= size || y < 0 || y >= size) {
	    return true;
	} else {
	    return cell(x,y) == WALL;
	}
    }

    public boolean hiddenIsBlocked(int x, int y) {
	return hiddenMaze[y][x] == WALL;
    }

    public boolean isGoal(int x, int y) {
	return cell(x,y) == GOAL;
    }

    public State goalState() {
	return state(goalX, goalY);
    }

    public State startState() {
	return state(startX, startY);
    }

    public int cell(int x, int y) {
	return maze[y][x];
    }
    
    public String toString() {
	String str = "";
	for (int x = 0; x < size; ++x) {
	    for (int y = 0; y < size; ++y) {
		if (hiddenMaze[x][y] == WALL) {
		    str += "X";
		} else if (hiddenMaze[x][y] == GOAL) {
		    str += "T";
		} else if (hiddenMaze[x][y] == START) {
		    str += "A";
		} else {
		    str += " ";
		}
	    }
	    str += "\n";
	}
	return str;
    }

    public State state(int x, int y) {
	if (states[x][y] == null) {
	    states[x][y] = new State();
	    states[x][y].x = x;
	    states[x][y].y = y;
	}
	return states[x][y];
    }

}  
