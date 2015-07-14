class PathFinder {
    public Search search;
    protected State _location, goal;
    protected Maze maze;
    
    protected int searches = 0;

    public int expanded = 0;

    private ArrayList pathTaken;
    
    public PathFinder(Maze m) {
	pathTaken = new ArrayList();

	this.maze = m;

	_location = maze.startState();

	goal = maze.goalState();

	maze.fillIn(_location);

	findNewPath();
    }

    public boolean reached() {
	return _location == goal;
    }
    
    public boolean step() {
	if (maze.isGoal(_location.x, _location.y)) return true;

	pathTaken.add(_location);

	_location = nextStep();

	// couldn't find where to go next
	if (_location == null) return false;

	maze.fillIn(_location);
//	println("moving to "+_location);

	return true;
    }

    public State location() {
	return _location;
    }

    public Iterator plannedPath() {
	return goal.path().iterator();
    }

    protected State nextStep() {
	State state = goal;
	while (state.tree != _location) {
	    state = state.tree;
	}
	if (maze.isBlocked(state.x, state.y)) {
	    if (findNewPath()) {
		state = nextStep();
	    } else {
		state = null;
	    }
	}
	return state;
    }

    protected boolean findNewPath() {
	if (search != null) expanded += search.expanded;

	searches += 1;
	return restartSearch();
    }

    protected boolean restartSearch() {
	// this is the new starting point for searches
	_location.tree = null;
	search = new Search(_location, goal, searches, maze);
	return search.solve();
    }

    public Iterator actualPath() {
	return pathTaken.iterator();
    }

    public int actualPathLength() {
	return pathTaken.size();
    }

}

class RepeatedBackwardPathFinder extends PathFinder {

    RepeatedBackwardPathFinder(Maze m) {
	super(m);
    }
    
    protected boolean restartSearch() {
	for (int i = 0; i < maze.size; ++i) {
	    for (int j = 0; j < maze.size; ++j) {
		maze.state(i,j).setH(0);
	    }
	}
	search = new Search(goal, _location, searches, maze);
	return search.solve(); 
    }

    protected State nextStep() {
	State state = _location.tree;
	if (state == null || maze.isBlocked(state.x, state.y)) {
	    if (findNewPath()) {
		state = nextStep();
	    } else {
		state = null;
	    }
	}
	return state;
    }

    public Iterator plannedPath() {
	ArrayList list = new ArrayList();
	State state = _location;
	while (state != null) {
	    list.add(0, state);
	    state = state.tree;
	}
	return list.iterator();
    }

}

class AdaptivePathFinder extends PathFinder {

    public AdaptivePathFinder(Maze m) {
	super(m);
    }

    protected boolean restartSearch() {
	_location.tree = null;
	search = new Search(_location, goal, searches, maze);
	return search.solve(true);
    }

}