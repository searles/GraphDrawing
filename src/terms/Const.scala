package terms

case class Const[A](val sym: A) extends Leaf {
	override def vars: Set[String] = Set.empty
	override def syms: Set[Any] = Set(sym)

	override def toString(): String = sym.toString;

	override def applyClosure(sigma: Substitution, vars:Set[String]): Option[Term] = Some(this)
	
	override def apply(sigma: Substitution): Term = this;
	override def apply(x: String, t: Term): Term = this;

	override def unifier(t: Term, sigma: Substitution): Option[Substitution] = 
		t match {
		case Const(u) => if ( sym == u ) Some(sigma) else None
		case Var(_) => t unifier(this, sigma)
		case _ => None
	}
	
	override def matcher(t: Term, sigma: Substitution) = 
			if ( this == t ) Some(sigma) else None ;
	
	override def overlaps(t: Term): List[(List[Int], Substitution)] = 
	unifier(t) match {
		case Some(sigma) => List((Nil, sigma))
		case None => Nil
	}
}