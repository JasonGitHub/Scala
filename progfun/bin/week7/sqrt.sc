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
	curtStream(8).take(10).toList             //> res0: List[Double] = List(1.0, 3.3333333333333335, 2.462222222222222, 2.0813
                                                  //| 41247671579, 2.003137499141287, 2.000004911675504, 2.000000000012062, 2.0, 2
                                                  //| .0, 2.0)
}