package doubletake.lang.java;

import java.util.HashMap;

import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTParser;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
 
public class Visitor extends ASTVisitor {
    private CompilationUnit cu;
    private HashMap<SimpleName, Variable> data;

    public Visitor(CompilationUnit u) {
    	this.data = new HashMap<SimpleName, Variable>();
        this.cu = u;
    }
    
    public HashMap<SimpleName, Variable> getData() {
        return this.data;
    }

    private void logDef(SimpleName n) {
    	Variable var = this.data.get(n);
        if(var == null){
        	var = new Variable(n);
        	this.data.put(n, var);
        }
        var.addDef(this.cu.lineNumber(n.getStartPosition()));
    }
    
    private void logUse(SimpleName n) {
    	
    }

    public boolean visit(VariableDeclarationFragment node) {
        SimpleName name = node.getName();
        logDef(name);
        
        System.out.println("Declaration of '"+
        					name+
        					"' at line"+
        					this.cu.lineNumber(name.getStartPosition()));
        return false; // do not continue to avoid usage info
    }

    public boolean visit(SimpleName node) {
        logUse(node);
        System.out.println("Usage of '" +
        				   node + 
        				   "' at line " + 
        				   this.cu.lineNumber(node.getStartPosition()));
        return true;
    }
    
    public static Visitor run(String code) {
        ASTParser parser = ASTParser.newParser(AST.JLS3);
        parser.setSource(code.toCharArray());
        parser.setKind(ASTParser.K_COMPILATION_UNIT);        
        CompilationUnit cu = (CompilationUnit) parser.createAST(null);
        Visitor v = new Visitor(cu);
        cu.accept(v);
        return v;
    }
}
