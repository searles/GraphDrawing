package terms

case class Abs(id: String, expr: Term) extends Term {
	override def toString = "\\" + id + "." + expr
	
	override def app(r: Term): Term = {
		expr.apply(id, r) // beta reduction
	}
	
	override def root = this

	override def vars: Set[String] = expr.vars - id
	
	override def syms: Set[Any] = expr.syms
	
	override def at(p: Int) = p match {
		case 0 => expr
		case _ => throw new IllegalArgumentException()
	}
	
	override def replace(t: Term, p: Int) = p match {
		case 0 => t.abs(id)
		case _ => throw new IllegalArgumentException()
	}
	// Applying substitutions
	override def apply(sigma: Substitution) = expr.apply(sigma - id).abs(id)
	override def apply(x: String, t: Term) = if(id == x) this else expr.apply(x, t).abs(id) // TODO: Rename!
	
	// For the occur check we keep a set of variables.
	override def applyClosure(sigma: Substitution, vars: Set[String]) = throw new IllegalArgumentException("not implemented") // TODO

	// Unification and matching
	override def unifier(t: Term, sigma: Substitution): Option[Substitution] = throw new IllegalArgumentException("not implemented")  // TODO
	
	override def matcher(t: Term, sigma: Substitution): Option[Substitution] = throw new IllegalArgumentException("not implemented") // TODO
	
	override def equals(o: Any) = o match { // requires alpha-conversion.
		case Abs(id2, expr2) => if(id != id2) {
			expr.equals(expr2.apply(id2, Var(id)))
		} else {
			expr.equals(expr2)
		}
		case _ => false
	}

	// Finds all non-variable positions that are unifiable with t and returns the
	// corresponding matchers.
	override def overlaps(t: Term) = throw new IllegalArgumentException("not implemented")
	
	override def subterms(rewriteFunction: Term => Iterable[Term]) = rewriteFunction(expr).map(_.abs(id))
	override def parallel(rewriteFunction: Term => Iterable[Term]) = subterms(rewriteFunction)
	override def fullyParallel(rewriteFunction: Term => Iterable[Term]) = subterms(rewriteFunction)
}