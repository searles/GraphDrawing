package terms

case class Var(id: String) extends Leaf {
	
	override def vars: Set[String] = Set(id)
	override def syms: Set[Any] = Set.empty

	override def toString(): String = "'" + id;

	override def applyClosure(sigma: Substitution, vars:Set[String]): Option[Term] = 
	if ( vars contains id ) None else 
		sigma.get(id) match {
		case Some(t) => t applyClosure ( sigma, vars + id)
		case None => Some(this)
	}

	override def apply(sigma: Substitution): Term = sigma.get(id) match {
		case Some(t) => t
		case None => this
	}

	override def apply(x: String, t: Term): Term = if (id == x) t else this

	override def unifier(t: Term, sigma: Substitution): Option[Substitution] = 
		sigma.get(id) match {
		case Some(s) => if(s == t) Some(sigma) else s.unifier(t, sigma)
		case None => if(this == t) Some(sigma) else 
			if(canEqual(t)) Some(sigma + (id -> t)) else None
	}
	
	override def matcher(t: Term, sigma: Substitution) = 
		sigma.get(id) match {
		case Some(s) => if(s == t) Some(sigma) else None
		case None if canEqual(t) => Some(sigma + (id -> t))
		case _ => None
	}
	
	// For subclasses of Var that may want to check some properties of t
	// like types
	def canEqual(t: Term): Boolean = true
	
	override def overlaps(t: Term): List[(List[Int], Substitution)] = Nil
}