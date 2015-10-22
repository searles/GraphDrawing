package application

import javafx.scene.control.ScrollPane
import javafx.scene.Group
import graph.Graph
import terms.Term
import graph.SelectionHandler
import javafx.scene.input.MouseEvent
import javafx.scene.layout.GridPane
import javafx.scene.control.Button
import javafx.scene.control.CheckBox
import javafx.scene.control.Separator
import javafx.event.EventHandler
import javafx.event.ActionEvent
import javafx.scene.control.Label

class GraphPane(rewriteFunctionWrapper: () => Option[Term => Iterable[Term]]) extends ScrollPane {
	val graph = new Graph[Term]
	val group = new Group
	
	val handler = new SelectionHandler[Term] {
		override def setSelected(t: Term, e: MouseEvent) {
			rewriteTerm(t, 1)
		}
	}
	
	setContent(group)
	
	def clear { graph.clear; group.getChildren().clear }

	def addTerm(t: Term) {
		graph.add(t)
		group.getChildren.clear
		graph.draw(group, handler)
	}
	
	def rewriteTerm(t: Term, depth: Int) {
		rewriteFunctionWrapper() match {
			case Some(rewriteFunction) => {
				val tree = t.tree(rewriteFunction, depth)
				tree.map({ 
					case (s, ts) => {
						graph.disconnect(s)
						ts.map(graph.connect(s, _))
					}
				})
		
				group.getChildren.clear
				graph.draw(group, handler)
			}
			case _ => 
		}
	}
	
	def deriveTerm(t: Term, u: Term, depth: Int) {
		rewriteFunctionWrapper() match {
			case Some(rewriteFunction) => {
				val path = t.rewriteTo(u, rewriteFunction, depth)
				
				if(!path.isEmpty) {
					path.foreach(graph.add(_))
				
					path.tail.foldLeft(path.head)(
						{case (t1, t2) => graph.connect(t1, t2); t2 }
					)
					group.getChildren.clear
					graph.draw(group, handler)
				}
				
			}
			case _ => 
		}
	}
	
	object Controller extends GridPane {
		val termField1 = new TermTextField
		val addButton = new Button("Add")
		val rewriteCheckBox = new CheckBox("Rewrite")
		val termField2 = new TermTextField
		val rewriteToButton = new Button("To")
		val depthLabel = new Label("Depth: ")
		val depthIntField = new IntTextField(1, 1, Integer.MAX_VALUE)
		val separator1 = new Separator
		val separator2 = new Separator
		val separator3 = new Separator
		val clearButton = new Button("Clear")
		
		add(addButton, 0, 0)
		add(termField1, 1, 0)
		add(rewriteCheckBox, 0, 1)
		add(separator1, 0, 2, 2, 1)
		add(rewriteToButton, 0, 3)
		add(termField2, 1, 3)
		add(separator2, 0, 4, 2, 1)
		add(depthLabel, 0, 5)
		add(depthIntField, 1, 5)
		add(separator3, 0, 6, 2, 1)
		add(clearButton, 0, 7)
		
		addButton.setOnAction(new EventHandler[ActionEvent] {
			override def handle(e: ActionEvent) {
				termField1.getTerm match {
					case Some(t) => if (rewriteCheckBox.isSelected()) {
						val depth = depthIntField.getValue
						rewriteTerm(t, depth)
					} else {
						addTerm(t)
					}
					case _ => 
				}
				
			}
		})
		
		rewriteToButton.setOnAction(new EventHandler[ActionEvent] {
			override def handle(e: ActionEvent) {
				(termField1.getTerm, termField2.getTerm) match {
					case (Some(t), Some(u)) => {
						val depth = depthIntField.getValue
						deriveTerm(t, u, depth)
					}
					case _ => 
				}
				
			}
		})
		
		clearButton.setOnAction(new EventHandler[ActionEvent] {
			override def handle(e: ActionEvent) {
				graph.clear
				group.getChildren.clear
			}
		})
	}
}