package terms.rewriting

import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.lexical.StdLexical
import terms.Term
import terms.Var
import terms.rewriting.conditional.Reducible
import terms.rewriting.conditional.Condition
import terms.rewriting.conditional.Joinable
import terms.Const

class RewritingParser extends StdTokenParsers {
	type Tokens = StdLexical
	val lexical = new StdLexical
	lexical.delimiters ++= Seq("\\", ".", "(", ")", ",", "+", "-", "*", "/", ":", "<", ">", "<=", "->", "-><-", "=", "++", ";");
	
	val vars = List[String]("x", "y", "z")
	
	def parseTerm(source: String): ParseResult[Term] = {
			val tokens = new lexical.Scanner(source)
			(phrase(expr)(tokens))
	}
	
	def parseTrs(source: String): ParseResult[TRS] = {
			val tokens = new lexical.Scanner(source)
			(phrase(rules)(tokens))
	}
	
	def parseRule(source: String): ParseResult[Rule] = {
			val tokens = new lexical.Scanner(source)
			(phrase(rule)(tokens))
	}
	
	def rules: Parser[TRS] = ((rule <~ ";")*) ^^ { new TRS(_) }
	
	def rule: Parser[Rule] = expr ~ "->" ~ expr ~ ("<=" ~> conditions?) ^^ {
		case l ~ "->" ~ r ~ c => c match {
			case Some(cs) => {
				cs.foldRight(new Rule(l, r)){(cond, rule) => rule.conditional(cond)}
			}
			case None => new Rule(l, r)
		}
	}
	
	def conditions: Parser[List[Condition]] = condition ~ (( "," ~> condition )*) ^^ {
		case c ~ cs => c :: cs
	}
	
	def condition: Parser[Condition] = expr ~ ("->" | "-><-") ~ expr ^^ {
		case s ~ "->" ~ t => new Reducible(s, t)
		case s ~ "-><-" ~ t => new Joinable(s, t)
	}
	
	def expr: Parser[Term] = app
	
	def op: Parser[Const[String]] = ("+" | "-" | "*" | "/" | "\\^" | "<" | ">" | "=" | "++" ) ^^ Const[String]
	
	/*def infix: Parser[Term] = prefix ~ ((op ~ prefix)*) ^^ { case head ~ tail =>
		tail.foldLeft(head)((l, r) => r match { case c ~ t => App(App(c, l), t) })
	}*/
	
	def prefix: Parser[Term] = (op*) ~ app ^^ { case h ~ t => 
		h.foldRight(t)((l, r) => l app r )
	}
	
	def app: Parser[Term] 	= term ~ (term*)  ^^ {
		case head ~ tail => 
			tail.foldLeft(head)((l, r) => l app r) 
	}
	
	def term: Parser[Term] = leaf | abs  | num | parens | op
	
	def abs: Parser[Term] 	= "\\" ~> ident ~ "." ~ expr 	^^ { case x ~ "." ~ t => t.abs(x) }
	def leaf: Parser[Term] 		= ident					^^ { case id => if (vars.exists(id.startsWith(_))) Var(id) else Const(id) }
	def num: Parser[Term]		= numericLit			^^ Const[String]
	def parens: Parser[Term] 	= "(" ~> expr <~ ")" 

	def arglist: Parser[List[Term]]	=	"(" ~> ((expr ~ (("," ~> expr)*) )?) <~ ")" ^^ { _ match {
		case Some(h ~ t) => h :: t
		case None => Nil
	}
}

	/* About that grammar
	 * 
	 * Question 1: How to distinguish variables from constants?
	 * Answer: Maintain a list of variables (all others are constants)
	 * term op term | 
	 * Question 2: infix ops? Well, enter them as postfix, right?
	 * 
	 * This makes it actually easy for priorities (or does it???)
	 * 
	 * Questing 3: what about ","? Well, treat similar to brackets, somehow...
	 * 
	 * [+-]a (+- a)*
	 * [/ *] b (/ * b)*
	 * [^] c (^ c)*
	 * [<>=] d (<>= d)*
	 * 
	 * 
	 */
	
	// How to interprete? 
	// It depends, but putting calculations into rewrite rules, why not?
	// x + y -> z where x is num and y is num and z is result also num
	// Only thing to do is to allow parser to parse complex numbers
	
	// Grammar rules: 
	// expr = 
	// 
	// app = term (term)*
	// term = leaf | ( expr )
}