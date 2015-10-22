package application

import javafx.scene.control.TextField
import terms.rewriting.RewritingParser
import terms.Term

class TermTextField extends TextField {
	val parser = new RewritingParser
	var status = ""
	
	def getTerm : Option[Term] = {
		parser.parseTerm(getText) match {
			case parser.Success(t, _) => Some(t)
			case parser.Failure(msg, _) => { status = msg ; None }
			case parser.Error(msg, _) => { status = msg ; None }
		}
	}
}