package testing

import terms.Substitution
import terms.rewriting.RewritingParser

case class Test1(s: String) {}
class Test2(s: String, t: String) {}

	object TermTests {
	def main(args: Array[String]): Unit = {
		val parser = new RewritingParser
		
		val t1 = parser.parseTerm("(g x)").get
		val t2 = parser.parseTerm("(g a)").get
		val a = parser.parseTerm("a").get

		val sigma = Substitution.empty + ("x" -> a);
		
		println(t1 matcher t2)
		println(t1 matcher(t2, sigma))
		
	}

}