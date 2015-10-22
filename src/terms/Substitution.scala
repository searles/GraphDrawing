package terms

import scala.collection.immutable.MapLike

case class Substitution(map: collection.immutable.TreeMap[String, Term]) {

	override def toString = 
		if(map.isEmpty) "{}" 
			else map.tail.foldLeft("{" + map.head._1 + " -> " + map.head._2)({
				case (s, (x, t)) => s + ", " + x + " -> " + t
				}) + "}"
	
	def this(map: Map[String, Term]) = this(collection.immutable.TreeMap(map.toArray:_*))
	//def this() = this(collection.immutable.TreeMap.empty)
	
	implicit def fromMap(map: Map[String, Term]) = new Substitution(map)

	// What do I need: Combining two substitutions should give an optional substitution
	// It should be possible to unify two substitutions
	// There should be a function that returns all variables
	//def combine(sigma: Substitution): Substitution;

	def domain = map.keySet
	def range = map.values.toSeq
	
	def get(s: String) = map.get(s)

	def closure: Option[Substitution] = None // TODO
	
	def +(sigma: Substitution) = new Substitution(this.map ++ sigma.map)
	def -(s: String) = new Substitution(this.map - s)	

	def +(entry: (String, Term)) = entry._2 match {
		case Var(id) if (id == entry._1) => this
		case _ => new Substitution(this.map + entry)
	}
}

object Substitution {
	def empty = new Substitution(collection.immutable.TreeMap.empty[String, Term])
	
	// The following function returns a substitution that renames all 
	// occurrences of blacklist in src by new variables that neither occur in blacklist or src
	def renaming(src: Set[String], blacklist: Set[String]): Substitution = {
		def findName(s: String): String = {
			// get non-numeric prefix of s
			val i = s.lastIndexWhere(_.isDigit)
			
			val prefix = i match {
				case -1 => s
				case _ => s take (i + 1)
			}
			
			Stream.from(1).find({case m => !(blacklist contains (prefix + m))}) match {
				case Some(n) => prefix + n
				case None => throw new IllegalArgumentException("bug in rename method");
			}
		}
		
		if(src.isEmpty) {
			new Substitution(Map.empty[String, Term])
		} else {
			val x = src.head
			
			if(blacklist contains x) {
				val newX = findName(x)
				renaming(src.tail, blacklist + newX) + (x -> new Var(newX))
			} else {
				renaming(src.tail, blacklist + x)
			}
		}
	}
	

}