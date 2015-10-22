package terms.rewriting

import scala.collection.mutable.LinkedHashSet
import terms.Term
import terms.Var

class TRS(rules: List[Rule]) {
	
	/** Applies the first rule possible to this term
	 * @param root
	 * @return
	 */
	def applyFirst(root: Term): Iterable[Term] = {
		rules.foreach(
			_.apply(root) match {
					case Some(u) => return Iterable(u)
					case None => 
			}
		)
			
		Iterable.empty[Term]
	}

	/** Applies all rules to the term
	 * @param root
	 * @return
	 */
	def apply(root: Term): Iterable[Term] = rules.foldLeft(LinkedHashSet.empty[Term])(
		(set, rule) => rule.apply(root) match {
			case Some(u) => set += u
			case None => set
		}
	)

	/** Applies all rules to the term using developments
	 * @param root
	 * @return
	 */
	def development(root: Term, rewriteFunction: Term => Iterable[Term]): Iterable[Term] = rules.flatMap(
		_.develop(root, rewriteFunction)
	)
	
	def narrow(root: Term) = root match {
		case v: Var => LinkedHashSet.empty[Term]
		case _ => fullNarrow(root)
	} 
	
	private def fullNarrow(root: Term) = rules.foldLeft(LinkedHashSet.empty[Term])(
			(set, rule) => rule.rename(root.vars).narrow(root) match {
				case Some(u) => set += u
				case None => set
			} 
	)
	
	def criticalPairs = {
		rules.flatMap({alpha => rules.flatMap({beta => alpha.criticalPair(beta)})})
	}
		


	
	// --- now the functions ---
	object RootFunction {
		val Default = (t: Term, _: Term => Iterable[Term]) => apply(t)
		val First = (t: Term, _: Term => Iterable[Term]) => applyFirst(t)
		val Development = (t: Term, rewriteFunction: (Term => Iterable[Term])) => development(t, rewriteFunction)
		val Narrowing = (t: Term, rewriteFunction: (Term => Iterable[Term])) => narrow(t)
	}
	
	// TODO The next ones should be static
	
	object SubtermFunction {
		val Single = (t: Term, rewriteFunction: (Term => Iterable[Term])) => t.subterms(rewriteFunction)
		val Parallel = (t: Term, rewriteFunction: (Term => Iterable[Term])) => t.parallel(rewriteFunction)	
		val FullyParallel = (t: Term, rewriteFunction: (Term => Iterable[Term])) => t.fullyParallel(rewriteFunction)
	}
	
	def rewriteFunction(
			rootFunction: ((Term, Term => Iterable[Term]) => Iterable[Term]), 
			subtermFunction: ((Term, Term => Iterable[Term]) => Iterable[Term])) = 
				(t: Term, rewriteFunction: (Term => Iterable[Term])) => rootFunction(t, rewriteFunction) ++ subtermFunction(t, rewriteFunction)
				
	def altRewriteFunction(
			firstFunction: ((Term, Term => Iterable[Term]) => Iterable[Term]), 
			secondFunction: ((Term, Term => Iterable[Term]) => Iterable[Term])) = 
				(t: Term, rewriteFunction: (Term => Iterable[Term])) => {
						val first = firstFunction(t, rewriteFunction)
						if(first.isEmpty)
							secondFunction(t, rewriteFunction)
						else
							first
					}

	
	def rewriteRelation(positionFunction: (Term, (Term => Iterable[Term])) => Iterable[Term]): Term => Iterable[Term] = {
		def rewriteFunction(t: Term): Iterable[Term] = positionFunction(t, rewriteFunction)
				
		rewriteFunction
	}
}