package terms.rewriting.conditional

import terms.Term
import terms.Substitution

case class Reducible(val l: Term, val r: Term, val origin: Term) extends Condition {
	def this(l: Term, r: Term) = this(l, r, l)
	
	override def vars: Set[String] = l.vars ++ r.vars
	override def syms: Set[Any] = l.syms ++ r.syms

	override def toString = l + " -> " + r
	
	override def apply(sigma: Substitution) = new Reducible(l.apply(sigma), r.apply(sigma))	

	// By using a cached rewrite system duplicates are avoided
	override def rewriteStep(rs: Term => Iterable[Term]) = {
		val next = l.rewriteStep(rs)
		next.map(new Reducible(_, r, origin))
	}

	override def solution = r matcher(l)
}