package graph

import scala.collection.MapLike
import scala.collection.immutable.Stack
import javafx.event.EventHandler
import javafx.geometry.VPos
import javafx.scene.Group
import javafx.scene.input.MouseEvent
import javafx.scene.paint.Color
import javafx.scene.shape.CubicCurveTo
import javafx.scene.shape.LineTo
import javafx.scene.shape.MoveTo
import javafx.scene.shape.Path
import javafx.scene.shape.PathElement
import javafx.scene.shape.Polygon
import javafx.scene.shape.Rectangle
import javafx.scene.text.Text
import scala.collection.mutable.ArrayBuffer

class Graph[T] /*(cmp : Comparator[T])*/ {
	val nodeMap = collection.mutable.LinkedHashMap.empty[T, Node]

	override def toString = {
		nodeMap.values.map(n => n.toString + " -> " + n.out.toString).toString
	}

	def clear {
		nodeMap.clear
	}
	
	def sinks = nodeMap.values.filter(_.out.isEmpty).map(_.value)
	def sources = nodeMap.values.filter(_.in.isEmpty).map(_.value)
	
	private def getNode(t: T): Node = nodeMap.getOrElseUpdate(t, new Node(t))	

	def add(t: T) { getNode(t) }
	def add(t: T, succ: Seq[T]) {
		getNode(t)
		succ.map(connect(t, _))
	}

	def connect(src: T, dst: T) {
		val u = getNode(src)
		val v = getNode(dst)

		if (!u.out.contains(v)) {
			u.out += v
			v.in += u
		}
	}
	
	def disconnect(t: T) {
		val n = getNode(t);
		n.out.map(m => m.in.remove(m.in.indexOf(n)))
		n.out.clear
	}
	
	def remove(t: T) {
		val n = getNode(t)
		n.out.map(m => m.in.remove(m.in.indexOf(n)))
		n.in.map(m => m.out.remove(m.out.indexOf(n)))
		nodeMap.remove(t)
	}
	
	def reduce(predicate: T => Boolean): Graph[T] = {
		val reduced = new Graph[T]

		def rec(t: T, node: Node, set: collection.mutable.Set[T]) {
			if(predicate(node.value)) {
				reduced.connect(t, node.value)
			} else if(!set.contains(node.value)) {
				// avoid loops here...
				set += node.value
				for(u <- node.out) rec(t, u, set)
			}
		}

		
		for(t <- nodeMap.keySet) {
			if(predicate(t)) {
				reduced.add(t)
				
				for(u <- nodeMap(t).out) rec(t, u, collection.mutable.Set.empty[T])
			}
		}
		
		reduced
	}
		//$$sasTODO remove nodes statisfying predicate by connecting in and outs. if no outs/ins, ignore (they vanish). nodeMap.keySet.toList
		//

	// Node Structure:
	// int index + lowlink = for scc
	// scc, level (for leveling)
	
	
	// What do I need of a node:
	// In-edges, out-edges
	// blind nodes for cycles (two for each cycle, one on level higher and one on level lower)
	// Function returning the double-position based on positions of ingoing/outcoming nodes
	// Then, first level using straight forward method
	// Then, replace back-links by blind nodes

	// Leveling algorithm: 
	// Step 1: Identify strongly connected components
	// Step 2: Level using integer as marks

	class Node(t: T) {
		val in = collection.mutable.ArrayBuffer.empty[Node]
		val out = collection.mutable.ArrayBuffer.empty[Node]

		override def toString = t.toString/* + out.foldLeft(" {")((s, n) => s + n.value.toString + " ") + "}"*/

		def value = t;
	}

	// We find an order of nodes that reflects the structure of the graph but
	// avoids cycles
	private def orderNodes(nodes: List[Node], below: collection.mutable.Map[Node, collection.mutable.Set[Node]]) {
		def in(n: Node) = n.in.intersect(nodes)
		def out(n: Node) = n.out.intersect(nodes)

	/** Identifies strongly connected components */
		def tarjan : collection.mutable.Map[Node, List[Node]] = {
			var index = 0
			
			val stack = collection.mutable.Stack.empty[Node]
			val indices = collection.mutable.Map.empty[Node, Int]
			val lowlink = collection.mutable.Map.empty[Node, Int]
			val sccs = collection.mutable.LinkedHashMap.empty[Node, List[Node]]
			
			def strongConnect(u: Node) {
				indices(u) = index
				lowlink(u) = index
				
				index += 1;
				
				stack.push(u)
	
				out(u).map(v => {
					if (!indices.contains(v)) {
						strongConnect(v)
						lowlink(u) = Math.min(lowlink(u), lowlink(v))
					} else if (stack.contains(v)) {
						lowlink(u) = Math.min(lowlink(u), indices(v))
					}
				})
	
				if (indices(u) == lowlink(u)) {
					// the stack contains a strongly connected component
					// TODO make the following functional
					var sccNodes = List.empty[Node]
	
					var v = stack.pop;
	
					while (u != v) {
						sccNodes = v :: sccNodes
						v = stack.pop
					}
					
					sccNodes = u :: sccNodes
					
					// Assign scc to nodes
					val scc = nodes.filter(sccNodes.contains(_)) // to keep order
					
					sccNodes.map(n => sccs += (n -> scc))
				}
			}
	
			nodes.map(u => {
				if (!indices.contains(u)) strongConnect(u)
			})
			
			return sccs
		}
		
		val sccs = tarjan
		
		// Put adjacent nodes into order that do not share scc
		nodes.foreach(
				n => out(n).filter(
						m => sccs(m) != sccs(n)
				).foreach(
						u => { below(n) += u }
				)
		)
		
		// TODO Avoid toSet-part.

		// Now, we need to put all sccs into order
		sccs.values.toSet[List[Node]].foreach(scc => {
			// Pick the first node of the scc
			val node = scc.head
			val rest = scc.tail
			
			if(!rest.isEmpty) {
				// Call recursively
				orderNodes(rest, below)
			
				val outInScc = scc.intersect(node.out)
				val inInScc = scc.intersect(node.in)

				// If there are more in than out put it below all outgoing
				if(inInScc.size < outInScc.size) {
					outInScc.foreach(m => {if(node != m) below(node) += m})
				} else {
					inInScc.foreach(m => {if(node != m) below(m) += node})
				}
			}
			
		})
	}
		
	def getLeveling : collection.Map[Node, Int] = {
		// Below will contain a dag spanning over the graph
		// It contains the "above"-relation, i.e., if the set
		// of a node is empty, then it is a root and should have level 0
		val below = collection.mutable.Map.empty[Node, collection.mutable.Set[Node]]
		
		nodeMap.values.foreach({below(_) = collection.mutable.LinkedHashSet.empty[Node]})
		
 		orderNodes(nodeMap.values.toList, below)
 		
 		// Create an equivalent above out of below
 		val above = collection.mutable.Map.empty[Node, collection.mutable.Set[Node]]
		nodeMap.values.foreach({above(_) = collection.mutable.LinkedHashSet.empty[Node]})
		
		below.foreach({case (up, downs) => downs.foreach { above(_) += up} })
		
 		// Order now contains a dag (but not mst) that we can level quite easily
 		val levels = collection.mutable.LinkedHashMap.empty[Node, Int]
		
		Stream.from(0).takeWhile( l => {
			// all which only have leveled elements above
			val levelable = above.filter(_._2.forall(levels.contains(_))).keys
			
			// are levelable
			levelable.foreach(levels(_) = l)
			
			// remove them from order
			levelable.foreach(above.remove(_))
			
			!above.isEmpty
		}).toList // TODO This is ugly...
 		
		// Use spanning tree to set level values
		/*val minLevel = levels.values.min
		
		levels.mapValues(minLevel + _)
		
		val maxLevel = levels.values.max
		
		// Returns all incoming nodes of n that are leveled above n 
		def above(n: Node) = {
			val l = levels(n)
			n.in.filter(levels(_) < l)
		}
		
		def below(n: Node) = {
			val l = levels(n)
			n.out.filter(levels(_) > l)
		}
		
		// retrieves compact level and returns incoming nodes
		// if a new level is assigned
		
		def compactLevel(level: Int, nodes: Iterable[Node]) = {
			nodes.filter(!below(_).isEmpty).map(
					n => levels(n) = below(n).map(levels(_)).min - 1
			)
		}
				
		def compressLevel(level: Int, nodes: Iterable[Node]) = {
			nodes.filter(!above(_).isEmpty).map(
					n => levels(n) = above(n).map(levels(_)).max + 1
			)
		}
		
		(0 to maxLevel).reverse.map(level => 
			compactLevel(level, nodeMap.values.filter(level == levels(_)))
		)
		
		(0 to maxLevel).map(level =>
			compressLevel(level, nodeMap.values.filter(level == levels(_)))
		)*/
		
		levels
	}
	
	

	def order(leveling: collection.Map[Node, Int]):  collection.Map[Node, Double] = {
		val matrix = (0 to leveling.values.max).foldLeft(ArrayBuffer.empty[ArrayBuffer[Node]])(
				(m, i) => (
						m += leveling.foldLeft(ArrayBuffer.empty[Node])(
						{ case (array, (n, l)) => if(l == i) 
							array += n 
						else 
							array
						}
				))
		)
		
		def pos(n: Node) = { 
			val row = matrix(leveling(n))
			(row.indexOf(n) + 1) / (row.size + 1)
		}

		// Closer nodes should be closer to their neighbors, or not?
		// Try different strategies.
		def preferredPosInOut(n: Node) = if(n.in.isEmpty && n.out.isEmpty) 0 else (n.in.map(pos(_)).sum + n.out.map(pos(_)).sum) / (n.in.size + n.out.size)
		def preferredPosIn(n: Node) = if(n.in.isEmpty) 0 else (n.in.map(pos(_)).sum) / (n.in.size)
		def preferredPosOut(n: Node) = if(n.out.isEmpty) 0 else (n.out.map(pos(_)).sum) / (n.out.size)
		def preferredPos(n: Node) = {
			val above = n.in.filter(leveling(_) - leveling(n) == 1) 
			val below = n.out.filter(leveling(n) - leveling(_) == 1)
			
			if(above.isEmpty && below.isEmpty) 0 else (above.map(pos(_)).sum + below.map(pos(_)).sum) / (above.size + below.size)
		}

		// three pass
		/*for(i <- 0 to matrix.size - 1) {
			matrix(i) = matrix(i).sortBy(preferredPosInOut(_))
		}*/
		
		// TODO: Create order here...
		
		val o = collection.mutable.Map.empty[Node, Double]
		
		matrix.map(row => {
			row.foldLeft(0)((i, n) => { 
				o(n) = (i.toDouble + 1) / (row.size.toDouble + 1)
				i + 1 
			})
		})
		
		o
	}
	
	def draw(g: Group, handler: SelectionHandler[T]) {
		val l = getLeveling
		val o = order(l)
		
		// Create vertices and dummies
		val vertices = collection.mutable.LinkedHashMap.empty[Node, Vertex]
			nodeMap.values.map(
				n => {
					vertices(n) = new Vertex(n, l(n))
				}
			)
		
		
		
		val edges = vertices.values.flatMap(_.initEdges(vertices))
		
		// Create matrix
		def pos(g: GraphElement) = g match {
			case Vertex(n, level) => o(n)
			case Dummy(src, dst, level) => (o(src) + o(dst)) / 2.0
		}
		
		// TODO: Better edge routing.
		val vs = (0 to l.values.max).map(
				// TODO: Maybe better to add all of this to other stuff?
				i => { 
					List.empty[GraphElement] ++ // so that the type is GraphElement
					vertices.values.filter(_.level == i) ++ 
					edges.flatMap(_.dummies.filter(_.level == i))
				}.toArray//TODO: Keep order. It has some meaning .sortWith((u, v) => pos(u) < pos(v))
		)
		
		// TODO Arrange elements in vs
		
		
		// Some values
		val gap = 64
		val node_distance = 8

		def width(row: Seq[GraphElement]) = row.map(_.width).sum + node_distance * (row.size - 1)
		
		val maxWidth = vs.map(width(_)).max
		
		// Get positions (we center everything)
		vs.foldLeft(0)((y0, row) => {
			val height = if(row.isEmpty) 10 else row.map(_.preferredHeight).max // TODO: Why can row be empty?
			row.foldLeft((maxWidth - width(row)) / 2)((x0, v) => {
				v.p0 = (x0, y0) // set point
				v.height = height // set height of row to all points
				x0 + v.width + node_distance
			})
			(y0 + height + gap).toInt
		})
		
		vertices.values.foreach(_.draw(g, handler))
		edges.foreach(_.draw(g))
	}
	
	
	
	
	

	
	// ------ The next one is to reduce edge crossings ------------

	/*def maxLevel = nodeMap.values.map(_.level).max // TODO: Lazy val.
	val nodeMatrix = collection.mutable.ArrayBuffer.empty[collection.mutable.ArrayBuffer[Node]]
	
	def initLevelMatrix {
		// create one row for each level and add it to nodes
		(0 to maxLevel).foreach(_ => {
			nodeMatrix += collection.mutable.ArrayBuffer.empty[Node]
		})
		
		nodeMap.values.map(n => {
			n.row = nodeMatrix(n.level)
			nodeMatrix(n.level) += n
		}
		)
	}

		
	// TODO: Now reduce crossings somehow...
	
	
	// ------------------ Finally the drawing-part ---------------
	def totalWidth = levels.map(_.width).max // TODO: This should rather be a lazy val somehow.
	var levels: IndexedSeq[Level] = null;
	var vertices: Iterable[Vertex] = null;
	
	def createGraph {
		// This would be great in sth like a constructor
		levels = (0 to maxLevel).map(new Level(_))
		vertices = nodeMap.values.map(_.vertex)
	}
	
	class Level(index: Int) {
		val row = collection.mutable.ArrayBuffer.empty[GraphElement]
		
		def add(e: GraphElement) = row += e
		
		def width = row.map(_.width).sum
		
		// get position of e in row
		def x0(e: GraphElement) = (totalWidth - width) / 2. + row.takeWhile(_ != e).map(_.width).sum
		def y0 = index * 128. + 128. // TODO: Header if there is a cycle
		def height = 48.
	}*/
	
	// graph elements will be sorted according to pos.
	abstract class GraphElement(level: Int) {
		def width: Double;
		def preferredHeight: Double
		var p0 = (0.0, 0.0)
		var height = -1.0
	}

	// next, edge crossing:
	// assign all nodes of a level to a list. define comparator based on
	// positions of in/out-edges and sort. identify cases of cycles

	case class Vertex(n: Node, level: Int) extends GraphElement(level) {
		val out = collection.mutable.ArrayBuffer.empty[Edge]
		val in = collection.mutable.ArrayBuffer.empty[Edge]
		
		lazy val text = new Text(n.value.toString)
		
		def initEdges(vertices: collection.Map[Node, Vertex]): Seq[Edge] =
			n.out.map(vertices(_)).map(new Edge(this, _))
		
		// TODO: Check different orders
		val indent = 6
		
		lazy val textwidth = text.getLayoutBounds.getWidth
		lazy val width = Math.max(textwidth, preferredHeight) + indent * 2
		lazy val preferredHeight = text.getLayoutBounds.getHeight + indent * 2
		lazy val rect = new Rectangle()
		
		/*def sortInOut() {
			// Sort order of nodes depending on relative position of destination/source of edge
			Comparator<GraphElement> cmp = (u, v) -> {
				if (u.levelIndex == levelIndex
						&& v.levelIndex == levelIndex) {
					int du = u.pos() - pos(); // < 0 if left of this, > 0
											// otherwise
					int dv = v.pos() - pos();

					if (du < 0) { // if du is left
						return dv > 0 ? -1 : dv - du;
					} else {
						return dv < 0 ? 1 : dv - du;
					}
				} else if (u.levelIndex == levelIndex) {
					// v is in above level
					return u.pos() - pos();
				} else if (v.levelIndex == levelIndex) {
					// u is in above level
					return pos() - v.pos();
				} else {
					// both are in top layer
					return u.pos() - v.pos();
				}
			};

			Collections.sort(in, cmp);
			Collections.sort(out, cmp);
		}*/
		
		def anchorOut(edge: Edge) = (p0._1 + (out.indexOf(edge) + 1) * width / (out.size + 1), p0._2 + height)
		def anchorIn(edge: Edge) = (p0._1 + (in.indexOf(edge) + 1) * width / (in.size + 1), p0._2)
		
		def draw(g: Group, handler: SelectionHandler[T]) {
			// Draw node + incoming arrows
			rect.setX(p0._1)
			rect.setWidth(width)
			rect.setY(p0._2)
			rect.setHeight(height)

			rect.setStroke(Color.BLACK)
			rect.setFill(Color.WHITE)
			
			rect.setOnMouseClicked(new EventHandler[MouseEvent] {
				def handle(e: MouseEvent) {
					handler.setSelected(n.value, e)
				}
			})

			text.setTextOrigin(VPos.CENTER)
			text.setX(rect.getX + rect.getWidth / 2.0 - textwidth / 2.0)
			text.setY(rect.getY + rect.getHeight / 2.0)
			text.setStroke(Color.BLACK)
			
			// TODO: Pass through to rect
			text.setOnMouseClicked(new EventHandler[MouseEvent] {
				def handle(e: MouseEvent) {
					handler.setSelected(n.value, e)
				}
			})

			g.getChildren.add(rect)
			g.getChildren.add(text)
		}
	}
	
	case class Dummy(src: Node, dst: Node, level: Int) extends GraphElement(level) {
		def width = 0;
		
		def anchorIn = (p0._1, p0._2)
		def anchorOut = (p0._1, p0._2 + height)
		
		def preferredHeight = 0.0
	}
	
	class Edge(src: Vertex, dst: Vertex) {
		
		src.out += this
		dst.in += this
		
		val dummies = {
			if(dst.level > src.level) // non-backwards
					((src.level + 1) to (dst.level - 1)).map(new Dummy(src.n, dst.n, _))
				else // backwards
					(dst.level to src.level).map(new Dummy(src.n, dst.n, _))

		}

		val cpDist = 32.0;
		
		def pathTo(p0: (Double, Double), p1: (Double, Double), shape: Int): PathElement =
		{
			val curve = new CubicCurveTo();
			
			curve.setControlX1(p0._1);
			curve.setControlY1(p0._2 + (if (shape == 1) -cpDist else cpDist));
			
			curve.setControlX2(p1._1);
			curve.setControlY2(p1._2 + (if (shape == 2) cpDist else -cpDist));
			
			curve.setX(p1._1);
			curve.setY(p1._2);
			
			return curve;
		}
		
		def draw(g: Group) {
			val path = new Path();
			
			val startPoint = src.anchorOut(this)
			val endPoint = dst.anchorIn(this)
				
			if(src.level >= dst.level) {
				// if cyclic draw in reverse
				path.getElements().add(new MoveTo(endPoint._1, endPoint._2));
				
				val lastPoint = dummies.foldLeft(endPoint)(
						(point, dummy) => {
							val prevIn = dummy.anchorIn
							val prevOut = dummy.anchorOut
							path.getElements.add(pathTo(point, prevIn, if(point == endPoint) 1 else 0))
							path.getElements.add(new LineTo(prevOut._1, prevOut._2))
							prevOut
						}
				)

				path.getElements.add(pathTo(lastPoint, startPoint, 2))
			} else {
				path.getElements().add(new MoveTo(startPoint._1, startPoint._2));
				
				val lastPoint = dummies.foldLeft(startPoint)(
						(point, dummy) => {
							val nextIn = dummy.anchorIn
							val nextOut = dummy.anchorOut
							path.getElements.add(pathTo(point, nextIn, if(point == endPoint) 1 else 0))
							path.getElements.add(new LineTo(nextOut._1, nextOut._2))
							nextOut
						}
				)
				
				path.getElements.add(pathTo(lastPoint, endPoint, 0))
			}
				
			path.setStroke(Color.BLACK.deriveColor(0,  0,  0,  0.75))
			path.setStrokeWidth(1.2)
			path.setFill(Color.TRANSPARENT)

			g.getChildren.add(path)
				
			// Add arrow head
			val arrowhead = new Polygon()
			arrowhead.getPoints.addAll(
					endPoint._1, endPoint._2,
					endPoint._1 - 2, endPoint._2 - 4,
					endPoint._1, endPoint._2 - 2,
					endPoint._1 + 2, endPoint._2 - 4
					)
			arrowhead.setStroke(Color.BLACK)
			arrowhead.setStrokeWidth(1)
			g.getChildren.add(arrowhead)
		}
	}

	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
		
	
	// ------------------------------------------------------------------------------------------------------------ //
	
	// The following methods are only for testing 
	// the properties of the graph.

	def nodes: java.util.Set[T] = {
		var set = new java.util.HashSet[T]()
		nodeMap.keys.map(set.add)

		set
	}

	def succ(t: T): java.util.Set[T] = {
		var set = new java.util.HashSet[T]()
		nodeMap.get(t) match {
			case Some(n) => n.out.map(m => set.add(m.value))
			case None =>
		}

		set
	}

	def isConnected(src: T, dst: T): Boolean = {
		var n = nodeMap.getOrElse(src, throw new IllegalArgumentException())

		var done = collection.mutable.HashSet.empty[Node]
		var stack = collection.mutable.Stack.empty[Node]

		stack.push(n)

		while (!stack.isEmpty) {
			var m = stack.pop;

			if (!done.contains(m)) {
				if (m.value == dst) return true;
				stack.pushAll(m.out);
				done += m;
			}
		}

		return false;
	}
	
	def jLeveling : java.util.LinkedHashMap[T, Integer] = {
		getLeveling.foldLeft(new java.util.LinkedHashMap[T, Integer])((map, entry) => { map.put(entry._1.value, entry._2); map })
	}
}
