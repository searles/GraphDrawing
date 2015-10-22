/*package math

import terms.App
import terms.Const
import terms.Term
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.lexical.StdLexical
import terms.Var

class MathParser extends StdTokenParsers {
	type Tokens = StdLexical
	val lexical = new StdLexical
	lexical.delimiters ++= Seq("\\", ".", "(", ")", ",", "+", "-", "*", "/", ":", "^");
	/* 
	 * expr = lambda | sum
	 * lambda = '\' id '.' expr
	 * sum = product ('+' | '-' product)*
	 * product = power ('*'|'/' power)*
	 * power = term(true) ('^' power)?
	 * term(mayHaveLeadingMinus) = termNoBrackets(true) | '(' expr ')'
	 * termNoBrackets(mayHaveLeadingMinus) = num(mayHaveLeadingMinus) | id arguments | var | typedvar
	 * arguments = termNoBrackets(false) | '(' expr (',' expr)* ')'
	 * 
	 * num(mayHaveLeadingMinus) = real(mayHaveLeadingMinus)(':' real(true))?
	 * real(mayHaveLeadingMinus) if(mayHaveLeadingMinus) dbl else posDbl
	 * var = '$' + id [ = Var(id) without $]
	 * cplxvar = '$$' + id
	 * 
	 *
	
	def expr: Parser[Term] = sum
	def lambda: Parser[Term] = "\\" ~> ident ~ "." ~ expr ^^ { case x ~ "." ~ t => t.abs(x) } 
	def sum: Parser[Term] = product ~ ((("+" | "-") ~ product)*) ^^ { 
		case t ~ ts => ts.foldLeft(t)({
			case (l, "+" ~ r) => new BinOp("+") { override def apply(l: Cplx, r: Cplx) = l + r}.app(l).app(r)
			case (l, "-" ~ r) => new BinOp("-") { override def apply(l: Cplx, r: Cplx) = l - r}.app(l).app(r)
	}) }
	def product: Parser[Term] = power ~ ((("*" | "/") ~ power)*) ^^ { 
		case t ~ ts => ts.foldLeft(t)({
			case (l, "*" ~ r) => new BinOp("*") { override def apply(l: Cplx, r: Cplx) = l * r}.app(l).app(r)
			case (l, "/" ~ r) => new BinOp("/") { override def apply(l: Cplx, r: Cplx) = l / r}.app(l).app(r)
	}) }
	def power: Parser[Term] = term(true) ~ (("^" ~> power)?) ^^ { 
		case l ~ rs => rs match {
			case Some(r) => new BinOp("^") { override def apply(l: Cplx, r: Cplx) = l ^ r}.app(l).app(r)
			case None => l
		}
	}
	def term(trailingMinusAllowed: Boolean): Parser[Term] = leaf(trailingMinusAllowed) | "(" ~> expr <~ ")"
	def leaf(trailingMinusAllowed: Boolean): Parser[Term] = num(trailingMinusAllowed) | lambda | fnapp | variable
	def fnapp: Parser[Term] = None
	def arguments: Parser[Term] = None
	def num(trailingMinusAllowed: Boolean): Parser[Const[Cplx]] = real(trailingMinusAllowed) ~ (":" ~> real(true))?
	def real(trailingMinusAllowed: Boolean): Parser[Double] = floatingPointNumber ^^ _.toDouble
	def variable: Parser[Term] = "$" ~> ("$"?) ~ ident ^^ {
		case Some(_) ~ id => new NumVar(id)
		case None ~ id => new Var(id)
	}

	
}

trait MathTerm extends Term {
	override def app(r: Term) = r match {
		case Const(sym) => this.app(List(sym)) match {
			case Some(result) => Const(result)
			case None => new MathApp(this, Const(sym))
		}
		case _ => super.app(r)
	}
	
	def app(args: List[Any]): Option[Any]
}

abstract class MathFun(id: String) extends Const[String](id) with MathTerm {
}

abstract class BinOp(id: String) extends MathFun(id) {
	def app(args: List[Any]): Option[Any] = args match {
		case (l: Cplx) :: (r: Cplx) :: Nil => Some(apply(l, r))
		case _ => None
	}
	
	def apply(l: Cplx, r: Cplx)
}

abstract class UnOp(id: String) extends MathFun(id) {
	def app(args: List[Any]): Option[Any] = args match {
		case (a: Cplx) :: Nil => Some(apply(a))
		case _ => None
	}
	
	def apply(a: Cplx)
}

class MathApp(l: MathTerm, r: Const[Any]) extends App(l, r) with MathTerm {
	def app(args: List[Any]): Option[Any] = l.app(r.sym :: args)
}

class NumVar(id: String) extends Var(id: String) {
	override def canEqual(t: Term) = t match {
		case Const(sym) => (sym.isInstanceOf[Cplx])
		case _ => false
	}
}*/*/