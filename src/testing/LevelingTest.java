package testing;

import graph.Graph;
import org.junit.Test;

import java.util.Map;
import java.util.Random;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class LevelingTest {

	boolean showGraph(Graph<?> g) {
		System.out.println(g);
		return false;
	}
	
	boolean show(String s) {
		System.out.println(s);
		return false;
	}
	
	Graph<String> graph1() {
		Graph<String> g = new Graph<String>();
		g.connect("a", "a");
		g.connect("a", "b");
		g.connect("a", "c");
		g.connect("b", "c");
		g.connect("b", "d");
		
		return g;
	}

	Graph<String> rwGraph() {
		Graph<String> g = new Graph<String>();
		
		
		g.add("fd");
		g.add("fc");
		
		
		g.connect("fc", "fk");
		g.connect("fd", "fk");
		g.connect("fc", "fe");
		g.connect("fc", "c");
		g.connect("fe", "e");
		g.connect("c", "k");
		g.connect("d", "k");
		g.connect("c", "e");

		System.out.println(g.toString());
		System.out.println(g.jLeveling());

		return g;
	}
	
	Graph<Integer> checkOrder() {
		Graph<Integer> g = new Graph<Integer>();
		g.add(0);
		g.add(3);
		g.add(2);
		g.add(1);
		
		g.connect(0, 1);
		g.connect(1, 2);
		g.connect(2, 1);
		g.connect(3, 2);
		
		return g;		
	}
	
	Graph<Integer> checkSCCLeveling() {
		Graph<Integer> g = new Graph<Integer>();
		g.connect(4, 0);
		g.connect(2, 3);
		g.connect(3, 1);
		g.connect(0, 1);
		g.connect(1, 0);
		
		return g;		
	}
	
	Graph<Integer> lastIsFirstLeveled() {
		Graph<Integer> g = new Graph<Integer>();
		g.add(5);
		g.add(4);
		g.add(3);
		g.add(2);
		g.add(1);
		g.add(0);
		
		g.connect(0, 1);
		g.connect(0, 2);
		g.connect(1, 2);
		g.connect(2, 3);
		g.connect(3, 4);
		g.connect(4, 1);
		
		g.connect(4, 5);
		
		return g;
	}
	
	Graph<Integer> testError() {
		Graph<Integer> g = new Graph<Integer>();
		// 0 -> 1
		// 1 -> 0,1,3
		g.connect(2, 0);
		g.connect(2, 4);
		g.connect(0, 1);
		g.connect(0, 4);
		g.connect(4, 0);
		
		return g;
	}
	
	<T> void checkGraphSimple(Graph<T> g) {
		Map<T, Integer> leveling = g.jLeveling();
		for(T s : g.nodes()) {
			assertTrue(leveling.get(s) >= 0 && leveling.get(s) < Integer.MAX_VALUE);
			
			for(T t : g.succ(s)) {
				if(leveling.get(t) <= leveling.get(s)) {
					// If they are not below, it must be a cycle
					assertTrue(g.isConnected(t, s) || show(t + " ->? " + s) || show(g.toString()) || show(leveling.toString()));
				}
			}
		}		
	}

	/*<T> void checkIntegrity(Graph<T> g) {		
		for(T s : g.nodes()) {
			for(T t : g.nodes()) {
				if(g.getLevel(s) >= g.getLevel(t) && g.isConnectedSCC(s, t) && s != t) {
					assertTrue(g.isConnected(t, s) || showGraph(g) || show(s + " -> " + t));
				}
			}
		}
	}*/

	
	Graph<Integer> rndGraph(int maxNodes, int edgeCount) {
		Graph<Integer> g = new Graph<Integer>();
		Random rnd = new Random();
		
		for(int i = 0; i < edgeCount; i++) {
			g.connect(Math.abs(rnd.nextInt()) % maxNodes, Math.abs(rnd.nextInt()) % maxNodes);
		}
		
		return g;
	}

	@Test(timeout=5000)
	public void basicTest1() {
		Graph<String> g = graph1();
		assertTrue(g.isConnected("a", "a"));

		assertTrue(g.isConnected("a", "a"));
		assertTrue(g.isConnected("a", "c"));
		assertFalse(g.isConnected("c", "d"));
		assertFalse(g.isConnected("b", "a"));
		
		checkGraphSimple(g);
	}
	
	@Test(timeout=5000)
	public void basicTest2() {
		Graph<String> g = rwGraph();
		checkGraphSimple(g);
	}
	
	@Test(timeout=5000)
	public void test() {
		Graph<Integer> g = checkOrder();
		checkGraphSimple(g);
	}
	
	@Test(timeout=5000)
	public void badCycle() {
		Graph<Integer> g2 = lastIsFirstLeveled();
		checkGraphSimple(g2);
	}
	
	@Test(timeout=5000)
	public void testErrorCyclic() {
		Graph<Integer> g2 = testError();
		checkGraphSimple(g2);
	}
	
	@Test(timeout=5000)
	public void testSCCLeveling() {
		Graph<Integer> g2 = checkSCCLeveling();
		checkGraphSimple(g2);
	}
	
	@Test(timeout=5000)
	public void stressTest() {
		Graph<Integer> g = rndGraph(4, 20);
		checkGraphSimple(g);
	}
}
