package terms

case class App(l: Term, r: Term) extends Term {
	
	override def vars: Set[String] = l.vars ++ r.vars
	override def syms: Set[Any] = l.syms ++ r.syms

	override def root = l.root

	override def abs(id: String) = r match {
		case Var(id) if(!l.vars.contains(id)) => l // eta-reduction
		case _ => new Abs(id, this)
	}
	
	override def at(p: Int): Term = p match {
		case 0 => l
		case 1 => r
		case _ => this
	}
	
	override def replace(t: Term, p: Int): Term = p match {
		case 0 => t app r
		case 1 => l app t
		case _ => t
	}
	
	override def applyClosure(sigma: Substitution, vars:Set[String]): Option[Term] =
		l applyClosure(sigma, vars) match {
		case Some(l2) => r applyClosure(sigma, vars) match {
			case Some(r2) => if((l eq l2) && (r eq r2)) Some(this) else Some(l2 app r2)
			case None => None
		}
		case None => None
	}
	
	override def toString(): String = (
			l match {
				case Abs(_, _) => "(" + l + ")"
				case _ => l.toString
			}) + " " + (
			r match {
				case App(_, _) => "(" + r + ")"
				case _ => r.toString
			})
	
	override def apply(sigma: Substitution): Term =
		(l apply(sigma), r apply(sigma)) match {
		case (l2, r2) => if((l eq l2) && (r eq r2)) this else l2 app r2
	}
	
	override def apply(x: String, t: Term): Term = 
		(l apply(x, t), r apply(x, t)) match {
		case (l2, r2) => if((l eq l2) && (r eq r2)) this else l2 app r2
	}

	override def unifier(t: Term, sigma: Substitution): Option[Substitution] = 
		t match {
		case App(l2, r2) => l unifier(l2, sigma) match {
			case Some(tau) => r unifier(r2, tau)
			case None => None
		}
		case Var(_) => t unifier(this, sigma)
		case _ => None
	}
	
	override def matcher(t: Term, sigma: Substitution): Option[Substitution] = t match {
		case App(l2, r2) => (l matcher(l2, sigma)) match {
			case Some(tau) => r matcher(r2, tau)
			case None => None
		}
		case _ => None
	}

	
	override def overlaps(t: Term): List[(List[Int], Substitution)] = {
		val lops = l.overlaps(t).map( { case(pos, sigma) => (0 :: pos, sigma)} )
		val rops = r.overlaps(t).map( { case(pos, sigma) => (1 :: pos, sigma)} )
		unifier(t) match {
			case Some(sigma) => (Nil, sigma) :: (lops ++ rops)
			case None => (lops ++ rops)
		}
	}

	// How to rewrite subterms
	override def subterms(rewriteFunction: Term => Iterable[Term]): Iterable[Term] = {
		val lreducts = rewriteFunction(l)
		val rreducts = rewriteFunction(r)
		
		if(lreducts.isEmpty) // if to force right order
			rreducts.map(l app _)
		else
			lreducts.map(_ app r) ++ rreducts.map(l app _) 
	}

	override def parallel(rewriteFunction: Term => Iterable[Term]): Iterable[Term] = {
		val lreducts = rewriteFunction(l)
		val rreducts = rewriteFunction(r)
		
		if(lreducts.isEmpty) 
			rreducts.map(l app _)
		else if(rreducts.isEmpty)
			lreducts.map(_ app r)
		else
			(lreducts.flatMap(l => rreducts.map(r => l app r)) ++ lreducts.map(_ app r) ++ rreducts.map(l app _))
	}
	
	override def fullyParallel(rewriteFunction: Term => Iterable[Term]): Iterable[Term] = {
		val lreducts = rewriteFunction(l)
		val rreducts = rewriteFunction(r)
		
		if(lreducts.isEmpty) 
			rreducts.map(l app _)
		else if(rreducts.isEmpty)
			lreducts.map(_ app r)
		else
			lreducts.flatMap(l => rreducts.map(r => l app r))
	}
}