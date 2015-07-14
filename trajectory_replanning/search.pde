class Search {
    private ArrayList open;
    private ArrayList closed;

    public Maze maze;

    public State goal;

    public int counter;

    public int expanded = 0;

    public Search(State start, State goal, int counter, Maze m) {
	maze = m;

	this.goal = goal;
	this.counter = counter;

	open = new ArrayList();

	start.setG(0);
	start.setSearch(counter);

	goal.setG(Number.MAX_VALUE);
	goal.setSearch(counter);
	goal.setH(0);

	addToOpenList(start);

	closed = new ArrayList();
    }

    public boolean solve() {
	return solve(false);
    }

    public boolean solve(boolean updateH) {
	State state;
	ArrayList successors, unknown;
	state = getFromOpenList();
	goal.setG(Number.MAX_VALUE);
	while (state != null) {
	    closed.add(state);
	    
//	    println("expanding "+state);
	    if (state == goal) {
		if (updateH) updateH(state);
//		println("GOAL!");
		return true;	
	    } else {
		expand(state);
	    }
	    state = getFromOpenList();
	}

	return false;
    }

    private void updateH(State goal) {
	State s;
//	println("updating h from "+goal);
	for (int i = 0; i < closed.size(); ++i) {
	    s = (State) closed.get(i);
//	    println("new h="+goal.g()+"-"+ s.g()+"="+(goal.g() - s.g()));	    
	    s.setH(goal.g() - s.g());
//	    println("updating h for "+s);
	}
	
    }

    public void expand(State s) {
	expanded += 1;
	int x = s.x;
	int y = s.y;
	
	generate(s, x+1, y);
	generate(s, x-1, y);
	generate(s, x, y+1);
	generate(s, x, y-1);
	
//	println("openlist "+open.size()+" counter "+counter);
    }

    public void generate(State parent, int x, int y) {
	if (validState(parent, x, y)) {
	    State succ = maze.state(x, y);
	    if (succ.search() < counter) {
		succ.setG(Number.MAX_VALUE);
		succ.setSearch(counter);
	    }
	    if (succ.g() > parent.g() + 1) {
		succ.setG(parent.g() + 1);
		succ.setTree(parent);
		if (succ.h() == 0) succ.setH(abs(x - goal.x) + abs(y - goal.y));
		if (!inOpenList(succ)) {
//		    println("adding to open list "+succ);
		    addToOpenList(succ);
		}
	    }
	}
    }

    public boolean validState(State parent, int x, int y) {
	return !maze.isBlocked(x, y) && (parent.tree != maze.state(x, y) && closed.indexOf(maze.state(x, y)) == -1);
    }

    private void addToOpenList(State state) {
	open.add(state);
    }

    private State getFromOpenList() {
	if (open.isEmpty()) return null;

	int index = 0;
	State best = (State) open.get(index);
	State compare;
	for (int i = 1; i < open.size(); ++i) {
	    compare = (State) open.get(i);
//	    println("comparing "+compare+" to "+best);
	    if (compare.f() <= best.f() ||
		(best.f() == compare.f() && compare.g() > best.g())) {// || 
//		(best.f() == compare.f() && compare.g() == best.g() && compare.h() < best.h())) {
//		println("found better");
		best = compare;
		index = i;
	    }
	}
	
	State state = (State) open.remove(index);

	return state;
    }

    private boolean inOpenList(State state) {
	return open.indexOf(state) != -1;
    }

}