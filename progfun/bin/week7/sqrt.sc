package week7

object sqrt {
	def sqrtStream(x: Double): Stream[Double] = {
		def improve(guess: Double): Double = (guess + x / guess) / 2
		lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
		guesses
	}                                         //> sqrtStream: (x: Double)Stream[Double]
	
	def curtStream(x: Double): Stream[Double] = {
		def improve(guess: Double): Double = (2 * guess + x / (guess * guess)) / 3
		lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
		guesses
	}                                         //> curtStream: (x: Double)Stream[Double]
}