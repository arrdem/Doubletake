package doubletake.java.spec;

import java.util.HashMap;
import java.util.HashSet;
import java.util.ArrayList;

public class DependTree {
	protected String name;
	protected HashSet<String> depends;
	protected Object cases;
	
	public DependTree(String s, HashSet d, Object o) {
		this.name = s;
		this.depends = d;
		this.cases = o;
	}
	
	public static DependTree[] resolveDepends(HashMap<String, DependTree> scope,
											  DependTree root) throws Exception {
		
		HashSet<String> seen = new HashSet<String>();
		ArrayList<String> resolved = new ArrayList<String>();
		
		_resolveDepends(scope, root, seen, resolved);
		
		DependTree[] results = new DependTree[resolved.size()];
		String k = null;
		for(int i = 0; i < resolved.size(); i++) {
			k = resolved.get(i);
			results[i] = scope.get(k);
		}
		return results;
	}
	
	@SuppressWarnings("static-access")
	private static void _resolveDepends(HashMap<String, DependTree> scope,
										DependTree root,
										HashSet<String> seen,
										ArrayList<String> resolved 
										) throws Exception{
		System.out.println(root.name);
		seen.add(root.name);
		
		for(String edge: root.depends) {
			if(!resolved.contains(edge)) {
				if(seen.contains(edge)) {
					String error = new String("Circular reference detected: %s -&gt; %s");
					throw new Exception(error.format(root.name, edge));
				} else {
					_resolveDepends(scope, scope.get(edge), seen, resolved);
				}
			}
			resolved.add(edge);
		}
	}
}
