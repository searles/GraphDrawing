package application

import javafx.scene.control.ScrollPane
import javafx.scene.control.TextArea
import javafx.beans.value.ChangeListener
import javafx.beans.value.ObservableValue
import terms.rewriting.TRS
import scala.collection.mutable.ListBuffer
import terms.rewriting.RewritingParser
import javafx.scene.layout.BorderPane
import javafx.scene.text.Text
import javafx.scene.layout.GridPane
import javafx.scene.control.Separator
import javafx.scene.control.CheckBox
import javafx.scene.control.ToggleButton
import javafx.scene.control.ToggleGroup
import javafx.scene.control.RadioButton
import javafx.scene.layout.VBox
import terms.Term
import terms.rewriting.Rule

class RewriteRelationPane extends BorderPane {
	object TRSEditor extends BorderPane {
		
		var trs: TRS = null
	
		val parser = new RewritingParser
	
		val status = new Text
		val editor = new TextArea

		editor.setMinSize(2.0, 12.0);
		editor.setPrefColumnCount(12);
		editor.setPromptText("Please enter rules");
		editor.textProperty().addListener(new ChangeListener[String]() {
			def changed(observable: ObservableValue[_ <: String], oldValue: String, newValue: String) {
				trs = null
				status.setText("")
			}
		}) 
		
		// TODO: Nice syntax for lambda terms
		
		//editor.setText("a -> c\na -> d\nb -> c\nb -> d\nc -> e\nc -> k\nd -> k\nd -> m\nf x -> x <= x -> e\ng x x -> h x x\neq x x -> true\n")
		//editor.setText("d * -> \\x.\\y.* (d x) y + * (d y) x\nd + -> \\x.\\y.+ (d x) (d y)\nd num -> 0\nd 0 -> 0\nd var -> num\nd (x y) -> (d x) (d y)\n")
		//editor.setText("a -> c\na -> d\nb -> c\nb -> d\ns k -> t a\ns l -> t a\nk -> m\nl -> m\nf x y -> tup x y <= s x -> t y\ng x x -> h x x\n");
		editor.setText("father a b -> true\nfather c d -> true\nfather b c -> true\ngfather x z -> true <= father x y -> true, father y z -> true\n");
		
		setCenter(editor)
		setBottom(status)
		
		def getTRS : Option[TRS] = {
			if(trs == null) {
				val text = editor.getText
				val lines = text.split("\\r?\\n")
				
				val errorMsg = collection.mutable.Map.empty[Int, String]
				val rules = ListBuffer.empty[Rule]
				
				for(i <- 0 to lines.length - 1) {
					val line = lines(i).trim
					if(!line.isEmpty) parser.parseRule(lines(i)) match {
						case parser.Success(rule, _) => {
							rules += rule
						}
						case parser.Failure(msg, _) => {
							errorMsg += (i -> msg)
						}
						case parser.Error(msg, _) => {
							errorMsg += (i -> msg)
						}
					}
				}
	
				if(!errorMsg.isEmpty) {
					status.setText(errorMsg.foldLeft("")({ 
						case (report, (i, msg)) => report + "line " + i + ": " + msg + "\n" 
					}))
				} else {
					trs = new TRS(rules.toList)
					println(trs.criticalPairs)
				}
			}
	
			if(trs == null) {
				None
			} else {
				Some(trs)
			}
		}
	}
	
	object StrategyPane extends VBox {
		val rootGroup = new ToggleGroup
		
		val defaultButton = new RadioButton("Default")
		val firstButton = new RadioButton("First")
		val developmentButton = new RadioButton("Development")
		val narrowingButton = new RadioButton("Narrowing")
	
		val separator1 = new Separator
	
		val positionGroup = new ToggleGroup

		val allPositionsButton = new RadioButton("All positions")
		val innermostButton = new RadioButton("Innermost")
		val outermostButton = new RadioButton("Outermost")
		val normalizeButton = new RadioButton("Normalize")
		
		val separator2 = new Separator
		
		val parallelGroup = new ToggleGroup
		
		val singleButton = new RadioButton("Single")
		val parallelButton = new RadioButton("Parallel")
		val fullyParallelButton = new RadioButton("Fully Parallel")

		List(defaultButton, firstButton, developmentButton, narrowingButton).foreach(_.setToggleGroup(rootGroup))
		List(allPositionsButton, innermostButton, outermostButton, normalizeButton).foreach(_.setToggleGroup(positionGroup))
		List(singleButton, parallelButton, fullyParallelButton).foreach(_.setToggleGroup(parallelGroup))
		
		List(defaultButton, allPositionsButton, singleButton).foreach(_.setSelected(true))

		this.getChildren().addAll(defaultButton, firstButton, developmentButton, narrowingButton, separator1, 
				allPositionsButton, innermostButton, outermostButton, normalizeButton, separator2, 
				singleButton, parallelButton, fullyParallelButton)
		
		// TODO: This should be in some other class
		def getRR(trs: TRS): Term => Iterable[Term] = {
			val rootFunction: (Term, Term => Iterable[Term]) => Iterable[Term] = 
				if(defaultButton.isSelected()) 
					//(t: Term, _: Term => Iterable[Term]) => trs.apply(t)
					trs.RootFunction.Default
				else if(firstButton.isSelected())
					//(t: Term, _: Term => Iterable[Term]) => trs.applyFirst(t)
					trs.RootFunction.First
				else if(developmentButton.isSelected())
					//(t: Term, rewriteFunction: (Term => Iterable[Term])) => trs.development(t, rewriteFunction)
					trs.RootFunction.Development
				else
					//(t: Term, rewriteFunction: (Term => Iterable[Term])) => trs.narrow(t)
					trs.RootFunction.Narrowing
			
			val subtermFunction: (Term, Term => Iterable[Term]) => Iterable[Term] = 
				if(singleButton.isSelected())
					trs.SubtermFunction.Single
					//(t: Term, rewriteFunction: (Term => Iterable[Term])) => t.subterms(rewriteFunction)
				else if(parallelButton.isSelected()) 
					//(t: Term, rewriteFunction: (Term => Iterable[Term])) => t.parallel(rewriteFunction)	
					trs.SubtermFunction.Parallel
				else 
					//(t: Term, rewriteFunction: (Term => Iterable[Term])) => t.fullyParallel(rewriteFunction)
					trs.SubtermFunction.FullyParallel
			
			val positionFunction: (Term, Term => Iterable[Term]) => Iterable[Term] = 
				if(allPositionsButton.isSelected()) 
					//(t: Term, rewriteFunction: (Term => Iterable[Term])) => rootFunction(t, rewriteFunction) ++ subtermFunction(t, rewriteFunction)
					trs.rewriteFunction(rootFunction, subtermFunction)
				else if(innermostButton.isSelected) 
					/*(t: Term, rewriteFunction: (Term => Iterable[Term])) => {
						val inner = subtermFunction(t, rewriteFunction)
						if(inner.isEmpty)
							rootFunction(t, rewriteFunction)
						else
							inner
					}*/
					trs.altRewriteFunction(subtermFunction, rootFunction)
				else if(outermostButton.isSelected) 
					/*(t: Term, rewriteFunction: (Term => Iterable[Term])) => {
						val outer = rootFunction(t, rewriteFunction)
						if(outer.isEmpty)
							subtermFunction(t, rewriteFunction)
						else
							outer
					}*/
					trs.altRewriteFunction(rootFunction, subtermFunction)
				else // Normalize; do not use this in non-terminating trss
					(t: Term, rewriteFunction: (Term => Iterable[Term])) => {
						def rewriteUntilNF(t: Term): Iterable[Term] = {
							println("testing " + t)
							val reducts = rewriteFunction(t)
							if(reducts.isEmpty) 
								Iterable(t)
							else
								reducts.flatMap(rewriteUntilNF)
						}

						val inner = subtermFunction(t, rewriteFunction)
						
						if(inner.isEmpty)
							rootFunction(t, rewriteFunction).flatMap(rewriteUntilNF)
						else {
							val reduced = inner.flatMap(rewriteUntilNF)
							
							if(reduced.isEmpty) 
								inner
							else 
								reduced
						}
					}
			
			def rewriteFunction(t: Term): Iterable[Term] = positionFunction(t, rewriteFunction)
				
			rewriteFunction
		}
	}
	
	setCenter(TRSEditor)
	setBottom(StrategyPane)
	
	def getRewriteRelation: Option[Term => Iterable[Term]] = {
		TRSEditor.getTRS match {
			case Some(trs) => Some(StrategyPane.getRR(trs))
			case None => None
		}
	}
}
