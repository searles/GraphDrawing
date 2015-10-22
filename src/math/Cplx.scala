package math

object Constants {
	val I = new Cplx(0, 1)
}

class Cplx(val re: Double, val im: Double) {
	
	def this(re: Double) = this(re, 0)
	implicit def fromDouble(d: Double): Cplx = new Cplx(d)
	
	def absSqr = re * re + im * im
	def abs = Math.hypot(im, re)
	
	def arg = {
		val arc = Math.atan2(im, re)
		if(arc < 0) {
			arc + 2 * Math.PI
		} else {
			arc
		}
	}

	def distSqr(c: Cplx) = {
		val dr = re - c.re
		val di = im - c.im
		
		dr * dr + di * di
	}

	// First binary things:
	def +(that: Cplx) = new Cplx(this.re + that.re, this.im + that.im)

	def -(that: Cplx) = new Cplx(this.re - that.re, this.im - that.im)

	def *(that: Cplx) = new Cplx(this.re * that.re - this.im * that.im, this.re * that.im + this.im * that.re)

	def /(that: Cplx) = {
		val d = that.absSqr
		new Cplx((this.re * that.re + this.im * that.im) / d, (-this.re * that.im + this.im * that.re) / d)
	}
	
	def unary_- = new Cplx(-re, -im)
	def unary_~ = { val d = absSqr; new Cplx(re / d, -im / d) }

	def sqrt = {
		val d = abs
		
		val r = Math.sqrt((d + re) / 2)
		var i = Math.sqrt((d - re) / 2)
		
		if(i < 0) i = -i
		
		new Cplx(r, i)
	}

	
	def conj = new Cplx(-re, im)
	
	def ^(that: Cplx) = {
		val hyp = abs
		
		if(hyp > 0) {
			val reLn = Math.log(hyp); // re for ln
			val imLn = Math.atan2(im, re); // im for ln
			
			// ln(a) * b
			val reExp = reLn * that.re - imLn * that.im
			val imExp = reLn * that.im + imLn * that.re
	
			// exp( ... )
			val ea = Math.exp(reExp)
			
			new Cplx(ea * Math.cos(imExp), ea * Math.sin(imExp))
		} else {
			if(that.re > 0) {
				0
			} else if(that.re == 0) {
				1
			} else {
				Double.PositiveInfinity
			}
		}
	}

	// Exp-stuff
	
	def exp = {
		val ea = Math.exp(re);
		new Cplx(ea * Math.cos(im), ea * Math.sin(im))
	}
	
	def log = new Cplx(Math.log(Math.hypot(re, im)), Math.atan2(im, re))

	// Trigonometric functions:
	def sin = {
		val d  = (this * Constants.I).exp
		(d - ~d) / (2.0 * Constants.I)
	}
	
	def cos = {
		val d  = (this * Constants.I).exp
		(d + ~d) / 2.0
	}
	
	def tan = {
		val z = (2.0 * Constants.I * this).exp
		(z - 1.0) / (Constants.I * (z + 1.0))
	}

	def sinh = {
		val d  = this.exp
		(d - ~d) / 2.0
	}
	
	def cosh = {
		val d  = this.exp
		(d + ~d) / 2.0
	}
	
	def tanh = {
		val z = (2.0 * this).exp
		(z - 1.0) / (z + 1.0)
	}
	
	def atan = ((1.0 + Constants.I * this) / (1.0 - Constants.I * this)).log / (2.0 * Constants.I)
	
	def atanh = ((1.0 + this) / (1.0 - this)).log / 2.0
}