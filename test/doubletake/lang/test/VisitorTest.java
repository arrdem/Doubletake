package doubletake.lang.test;

import static org.junit.Assert.*;

import org.junit.Test;

import doubletake.lang.java.Visitor;

public class VisitorTest {

	@Test
	public void test() {
		String code = "public class A { int i = 9;  \n int j; \n ArrayList<Integer> al = new ArrayList<Integer>();j=1000; }"
		Visitor v = Visitor.run(code);
		assertNotNull(v);
	}
}
