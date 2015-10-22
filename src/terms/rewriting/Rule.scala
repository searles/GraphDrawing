package terms.rewriting

import terms.Var
import terms.Substitution
import terms.rewriting.conditional.ConditionalTerm
import terms.Term
import terms.rewriting.conditional.Condition
import scala.collection.mutable.LinkedHashSet


case class Rule(lhs: Term, rhs: Term) {
	def rename(blacklist: Set[String]): Rule = 
		Substitution.renaming(lhs.vars ++ rhs.vars, blacklist) match {
			case sigma => Rule(lhs.apply(sigma), rhs.apply(sigma))
		}
	
	def vars: Set[String] = lhs.vars ++ rhs.vars

	def conditional(c: Condition) = new Rule(lhs, new ConditionalTerm(c, rhs))

	def matcher(t: Term): Option[Substitution] = 
		lhs matcher t

	def apply(t: Term): Option[Term] = matcher(t) match {
		case Some(sigma) => Some(rhs.apply(sigma))
		case None => None
	}
	
	private def apply(sigma: Substitution) = rhs apply sigma

	// Before using the following function we should rename so that variables are not shared
	def unifier(t: Term): Option[Substitution] = {
		if(vars.exists(t.vars.contains(_))) throw new IllegalArgumentException("Shared variables...");
		
		lhs unifier t
	}
	
	def narrow(t: Term) = {
		println("trying " + t + " and " + this)
		unifier(t) match {
			case Some(sigma) => Some(rhs.apply(sigma))
			case None => None 
		}
	}
	
	def develop(t: Term, rewriteFunction: Term => Iterable[Term]): Iterable[Term] = matcher(t) match {
		case Some(sigma) => expand(sigma, rewriteFunction).map(apply(_)) // TODO: Here we could insert other functions.
		case None => Iterable.empty[Term]
	}
	
	private def expand(matcher: Substitution, rewriteFunction: Term => Iterable[Term]) : Iterable[Substitution] = {
		// In beginning, only empty matcher
		matcher.domain.foldLeft(
			LinkedHashSet(Substitution.empty) //  We start with a set only containing the empty matcher
		)({
			case (sigmas, x) => {
				val taus = LinkedHashSet.empty[Substitution]
				
				val t = matcher.get(x) match {
					case Some(u) => u
					case None => throw new IllegalArgumentException();
				}
				
				// first add developed terms
				rewriteFunction(t).foreach(u => {
					sigmas.foreach(sigma => taus += (sigma + (x -> u)))
				})

				// Add original match for x at the end
				sigmas.foreach(sigma => taus += (sigma + (x -> t)))
				
				taus
			}
		}).toList
	}
	
	def criticalPair(rule: Rule): List[(Term, Term, Boolean)] = {
		val alpha = rule.rename(lhs.vars ++ rhs.vars)
		lhs.overlaps(alpha.lhs).map(
				{ case (pos, sigma) => (rhs.apply(sigma), lhs.replace(alpha.rhs, pos).apply(sigma), pos.isEmpty)})
	}
}