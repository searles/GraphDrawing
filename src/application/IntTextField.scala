package application

import javafx.scene.control.TextField

class IntTextField(default: Int, min: Int, max: Int) extends TextField {
	def getValue : Int = {
		try {
			val value = Integer.parseInt(getText)
			
			if(value < min) {
				setText(Integer.toString(min))
				min
			} else if(value > max) {
				setText(Integer.toString(max))
				max
			} else {
				value
			}
			
		} catch {
			case e : NumberFormatException => {
				setText(Integer.toString(default))
				default
			}
		}
	}
}