package terms.rewriting.conditional

import terms.Term
import terms.Substitution

trait Condition {
	def apply(sigma: Substitution): Condition
	def vars: Set[String]
	def syms: Set[Any]
	
	/** Performs one rewrite step and returns all next solvers and, if available, a solution
	 * representing the current state of the solver
	 * @param rs
	 * @return
	 */
	def rewriteStep(rs: Term => Iterable[Term]): Iterable[Condition]
	def solution: Option[Substitution]
}