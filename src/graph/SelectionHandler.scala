package graph

import javafx.scene.input.MouseEvent

trait SelectionHandler[-T] {
	def setSelected(t: T, e: MouseEvent) {}
}