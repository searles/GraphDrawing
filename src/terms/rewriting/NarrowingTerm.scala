package terms.rewriting

import terms.Substitution
import terms.Term

case class NarrowingTerm(t: Term, sigma: Substitution) extends Term {

	override def toString = t + " : " + sigma

	override def apply(sigma: Substitution) = {
		new NarrowingTerm(t, sigma) // todo
	}
	
	/*override def rewriteStep(rs: Term => Iterable[Term]): Iterable[Term] = {
		val nextSteps = c.rewriteStep(rs).map(nextC => new NarrowingTerm(context, pos, nextC, r))
		
		println(this + " -> " + nextSteps)
		
		// Is there a solution?
		c.solution match {
			case Some(tau) => nextSteps ++ Iterable(context.replace(r.apply(tau), pos)) // yes
			case None => nextSteps
		}
	}*/
	
	override def root = t.root
	
	// Build term and store info in decorator
	// decorator must not be a subterm 
	override def app(v: Term) = new NarrowingTerm(t.app(v.apply(sigma)), sigma)
	override def rApp(u: Term) = new NarrowingTerm(u.apply(sigma).app(t), sigma)
	
	// No rewriting in context
	override def subterms(applyToRoot: Term => Iterable[Term]): Iterable[Term] = applyToRoot(this)
	override def parallel(applyToRoot: Term => Iterable[Term]): Iterable[Term] = applyToRoot(this)
	override def fullyParallel(applyToRoot: Term => Iterable[Term]): Iterable[Term] = applyToRoot(this)

	override def at(p: Int) = t.at(p)
	
	override def vars = t.vars
	override def syms = t.syms
	
	override def replace(t: Term, p: Int): Term = new NarrowingTerm(t, sigma) // TODO
	
	override def apply(x: String, t: Term): Term = throw new IllegalArgumentException("Not supported")

	override def applyClosure(sigma: Substitution, vars: Set[String]): Option[Term] = throw new IllegalArgumentException("Not supported")

	override def unifier(t: Term, sigma: Substitution): Option[Substitution] = throw new IllegalArgumentException("Not supported")
	
	override def matcher(t: Term, sigma: Substitution): Option[Substitution] = throw new IllegalArgumentException("Not supported")
	
	override def overlaps(t: Term): List[(List[Int], Substitution)] = throw new IllegalArgumentException("Not supported")
}