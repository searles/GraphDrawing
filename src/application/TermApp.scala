package application

import javafx.scene.Scene
import javafx.scene.layout.BorderPane
import javafx.stage.Stage

class TermApp extends javafx.application.Application {

	val pane = new BorderPane();
	val rsPane = new RewriteRelationPane	
	val graphPane = new GraphPane(() => rsPane.getRewriteRelation)

	pane.setLeft(rsPane)
	pane.setRight(graphPane.Controller)
	pane.setCenter(graphPane)
	
	/*def rewriteTerm(t: Term, depth: Int) {
		val tree = t.tree(updateTRS.innermost, depth)
		tree.map({ 
			case (s, ts) => {
				graph.disconnect(s)
				ts.map(graph.connect(s, _))
			}
		})

		group.getChildren.clear
		// TODO: Filter graph - graph.reduce((t: Term) => (t.isInstanceOf[terms.App] || t.isInstanceOf[terms.Const] || t.isInstanceOf[terms.Var])).draw(group, handler)
		graph.draw(group, handler)
	}*/
	
	override def start(primaryStage: Stage) {
		try {
			val scene = new Scene(pane, 400, 400);
			
			//scene.getStylesheets().add(getClass().getResource("application.css").toExternalForm());
			primaryStage.setScene(scene);
			primaryStage.show();
		} catch {
			case e : Exception => e.printStackTrace();
		}
	}
}

object TermApp {
	def main(args: Array[String]) {
		javafx.application.Application.launch(classOf[TermApp], args: _*)
	}
}