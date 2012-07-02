package doubletake.lang.java;

import org.eclipse.jdt.core.dom.SimpleName;

public class Tripple {
	public int def;
	public int use;
	public SimpleName var;
	
	public Tripple(int d, int u, SimpleName v) {
		this.def = d;
		this.use = u;
		this.var = v;
	}
}
