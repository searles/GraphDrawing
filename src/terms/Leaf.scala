// Common ancestor  of Const and Var
package terms

import scala.collection.immutable.ListSet

trait Leaf extends Term {
	override def root = this

	override def at(p: Int): Term = {throw new IllegalArgumentException}
	
	override def replace(t: Term, p: Int): Term = {throw new IllegalArgumentException}

	override def subterms(rewriteFunction: Term => Iterable[Term]): Iterable[Term] = ListSet.empty[Term]
	override def parallel(rewriteFunction: Term => Iterable[Term]): Iterable[Term] = ListSet.empty[Term]
	override def fullyParallel(rewriteFunction: Term => Iterable[Term]): Iterable[Term] = ListSet.empty[Term]
}