package terms.rewriting.conditional

import terms.Term
import terms.Substitution
import terms.Const

// This is the right-hand side of conditional rules:
// It consists of r and c. If we find a matcher for this, we return the decorated term
// in which everything is propagated to the root position

// It is a decorator of a term, propagating the conditions to the outermost position

// TODO: Context should be handled in a different way
 /** Creates a conditional term, i.e., a term that is bound to a condition. In detail,
  * the right-hand side r is inserted into the context at position pos if c is satisfied.
 * @param c
 * @param r
 */
case class ConditionalTerm(context: Term, pos: List[Int], c: Condition, r: Term) extends Term {

	def this(c: Condition, r: Term) = this(new Const[String]("#"), List.empty[Int], c, r)
	
	override def toString = r + " <= " + c

	override def apply(sigma: Substitution) = new ConditionalTerm(context.apply(sigma), pos, c.apply(sigma), r.apply(sigma))
	
	override def rewriteStep(rs: Term => Iterable[Term]): Iterable[Term] = {
		// Get the next reducts
		val nextSteps = c.rewriteStep(rs).map(nextC => new ConditionalTerm(context, pos, nextC, r))
		
		// Is the condition satisfied?
		c.solution match {
			case Some(tau) => nextSteps ++ Iterable(context.replace(r.apply(tau), pos)) // yes
			case None => nextSteps
		}
	}
	
	override def root = context.root
	
	// Build term and store info in decorator
	// decorator must not be a subterm 
	override def app(v: Term) = new ConditionalTerm(context.app(v), 0 :: pos, c, r)
	override def rApp(u: Term) = new ConditionalTerm(u.app(context), 1 :: pos, c, r)
	
	// No rewriting in context
	// Theoretically, unravelings suggest that in certain cases
	// (linearity + sortedness) rewriting could be allowed
	override def subterms(applyToRoot: Term => Iterable[Term]): Iterable[Term] = applyToRoot(this)
	override def parallel(applyToRoot: Term => Iterable[Term]): Iterable[Term] = applyToRoot(this)
	override def fullyParallel(applyToRoot: Term => Iterable[Term]): Iterable[Term] = applyToRoot(this)

	override def at(p: Int) = context.at(p)
	
	override def vars = context.vars ++ c.vars ++ r.vars
	
	override def syms = context.syms ++ c.syms ++ r.syms
	
	override def replace(t: Term, p: Int): Term = new ConditionalTerm(context.replace(t, p), pos, c, r)
	
	override def apply(x: String, t: Term): Term = throw new IllegalArgumentException("Not supported")

	override def applyClosure(sigma: Substitution, vars: Set[String]): Option[Term] = throw new IllegalArgumentException("Not supported")

	override def unifier(t: Term, sigma: Substitution): Option[Substitution] = throw new IllegalArgumentException("Not supported")
	
	override def matcher(t: Term, sigma: Substitution): Option[Substitution] = throw new IllegalArgumentException("Not supported")
	
	override def overlaps(t: Term): List[(List[Int], Substitution)] = throw new IllegalArgumentException("Not supported")
}