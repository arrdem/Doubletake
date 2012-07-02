package doubletake.lang.java;

import java.util.ArrayList;

import org.eclipse.jdt.core.dom.SimpleName;

public class Variable {
	private SimpleName identifier;
	private ArrayList<Integer> defs;
	private ArrayList<Integer> uses;
	
	public Variable(SimpleName id) {
		this.identifier = id;
		this.defs = new ArrayList<Integer>();
		this.uses = new ArrayList<Integer>();
	}
	
	public void setIdentifier(SimpleName id) {
		this.identifier = id;
	}
	
	public void addUse(int lineno) {
		this.uses.add(new Integer(lineno));
	}
	
	public void addDef(int lineno) {
		this.defs.add(new Integer(lineno));
	}
}
