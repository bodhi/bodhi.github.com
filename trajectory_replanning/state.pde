class State {
    private int _g, _x, _y;
    private int _h;
    private State tree;
    private int _search;
    
    public State() {
	this._g = Number.MAX_VALUE;
	this._h = 0;
	this._search = 0;
    }

    public int compareTo(Object other) {
	State o = (State) other;
	return new Integer(h()).compareTo(new Integer(o.h()));
    }

    public int h() {
	return _h;
    }

    public void setH(int newH) {
	_h = newH;
    }

    public int g() {
	return _g;
    }

    public void setG(int newG) {
	_g = newG;
    }

    public int f() {
	return g() + h();
    }

    public int search() {
	return _search;
    }

    public void setSearch(int s) {
	_search = s;
    }
    
    public State tree() {
	return tree;
    }

    public void setTree(State tree) {
	this.tree = tree;
    }

    public ArrayList path() {
	ArrayList list = new ArrayList();
	State s = this;
	while (s.tree != null) {
//	    println(s);
	    list.add(0, s);
	    s = s.tree;
	}
	return list;
    }

    public String toString() {
	return _x+","+_y+" ("+g()+"+"+h()+"="+f()+")";
    }

}     
