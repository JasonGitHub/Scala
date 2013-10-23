package week2

object rationals {
 new Rational(1, 2)                               //> res0: week2.Rational = week2.Rational@5557c2bd
}

class Rational(x: Int, y: Int) {
	def numer = x
	def denom = y
}