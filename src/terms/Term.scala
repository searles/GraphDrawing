package terms

import scala.Option.option2Iterable
import scala.collection.JavaConverters._
import scala.collection.immutable.ListSet

trait Term {
	def app(r: Term): Term = r.rApp(this)
	protected def rApp(l: Term): Term = new App(l, this)
	
	def abs(id: String): Term = new Abs(id, this)

	/** The sole purpose of this function is to allow decorators to 
	 *  override it so that the rewrite function would be applied in
	 *  a 
	 * @param rewriteFunction
	 * @return
	 */
	def rewriteStep(rewriteFunction: Term => Iterable[Term]) = rewriteFunction(this)
	
		// --- Rewriting from here ---
	// Applies a function to all subterms
	// How to rewrite subterms
	def subterms(rewriteFunction: Term => Iterable[Term]): Iterable[Term]
	def parallel(rewriteFunction: Term => Iterable[Term]): Iterable[Term]
	def fullyParallel(rewriteFunction: Term => Iterable[Term]): Iterable[Term]
	

	// Some getters
	def vars: Set[String]
	def syms: Set[Any]
	def root: Term;
	
	def at(p: Int): Term;
	def at(pos: List[Int]): Term = pos match {
		case head :: tail => at(head).at(tail)
		case Nil => this
	}
	
	def replace(t: Term, p: Int): Term;
	def replace(t: Term, pos: List[Int]): Term = pos match {
		case head :: tail => replace(at(head).replace(t, tail), head)
		case Nil => t
	}

	// Applying substitutions
	def apply(sigma: Substitution): Term
	def apply(x: String, t: Term): Term
	
	// For the occur check we keep a set of variables.
	def applyClosure(sigma: Substitution, vars: Set[String]): Option[Term];
	def applyClosure(sigma: Substitution): Option[Term] = applyClosure(sigma, Set());

	// Unification and matching
	def unifier(t: Term, sigma: Substitution): Option[Substitution];
	def unifier(t: Term): Option[Substitution] = unifier(t, Substitution.empty);
	
	def matcher(t: Term, sigma: Substitution): Option[Substitution];	
	def matcher(t: Term): Option[Substitution] = matcher(t, Substitution.empty);
	
	// Finds all non-variable positions that are unifiable with t and returns the
	// corresponding matchers.
	def overlaps(t: Term): List[(List[Int], Substitution)]	

	// Stuff:
	def tree(rs: Term => Iterable[Term], depth: Int) = {
		val tr = collection.mutable.LinkedHashMap.empty[Term, Iterable[Term]]
		var set = collection.mutable.LinkedHashSet[Term](this)
		
		for(i <- 1 to depth) {
			val next = collection.mutable.LinkedHashSet.empty[Term]
			set.map(t => if(!tr.contains(t)){ 
				val reducts = t.rewriteStep(rs)
				tr(t) = reducts
				next ++= reducts
			})
			
			set = next
		}
		
		val filter1 = filterTerms(tr, { _ match {
			case App(_, _) => true
			case Var(_) => true
			case Const(_) => true
			case _ => false
		}})
		
		filterTerms(filter1, _.syms.forall({
			case s: String => !s.startsWith("U")
			case _ => true
		}))
		
		tr
	}
	
	def filterTerms(tree: collection.Map[Term, collection.Iterable[Term]], c: Term => Boolean): collection.Map[Term, collection.Iterable[Term]] = {
		
		def findNextTerms(t: Term, set: collection.mutable.Set[Term], marks: collection.mutable.LinkedHashSet[Term]) {
			if(!marks.contains(t)) {
				marks += t
				if(c(t)) {
					set += t
				} else {
					tree.get(t) match {
						case Some(ws) => ws.map(findNextTerms(_, set, marks))
						case None =>
					}
				}
			}
		}
		
		val returnMap = collection.mutable.LinkedHashMap.empty[Term, collection.Iterable[Term]]
		
		for(u <- tree.keySet) {
			if(c(u)) {
				val set = collection.mutable.LinkedHashSet.empty[Term]
				val marks = collection.mutable.LinkedHashSet.empty[Term]
				tree(u).map({v => findNextTerms(v, set, marks)})
				returnMap += (u -> set)
			}
		}
		
		returnMap
	}
	
	def rewriteTo(u: Term, rs: Term => Iterable[Term], depth: Int): List[Term] = {
		if(this == u)  return List(u)
		else if(depth == 0) return Nil
		else {
			rs(this).foreach(_.rewriteTo(u, rs, depth - 1) match {
				case Nil => 
				case list => return this :: list
			})
		}
		return Nil
	}
}