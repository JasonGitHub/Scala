package week0

object session {
	def eps = 1E-15                           //> eps: => Double

  def abs(x: Double): Double = if (x >= 0) x else -x
                                                  //> abs: (x: Double)Double
  
  def sqrt(x: Double): Double = {
    
    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double): Boolean =
      abs(guess * guess - x) / x < eps

    def improve(guess: Double): Double =
      (guess + x / guess) / 2

    sqrtIter(1.0)
  }                                               //> sqrt: (x: Double)Double
  sqrt(2)                                         //> res0: Double = 1.414213562373095
	
	def cbrt(x: Double): Double = {
		def cbrtIter(r: Double): Double = {
			if (abs(r * r * r - x) / x < eps) r
			else cbrtIter((x / (r * r) + 2 * r) / 3)
		}
		cbrtIter(1.0)
	}                                         //> cbrt: (x: Double)Double
	
	cbrt(2)                                   //> res1: Double = 1.2599210498948732
	
	def gcd(a: Int, b: Int): Int = {
		if (b == 0) a else gcd(b, a % b)
	}                                         //> gcd: (a: Int, b: Int)Int
	
	def factorial(n: Int): Int = {
		if (n == 0) 1 else n * factorial(n - 1)
	}                                         //> factorial: (n: Int)Int
	
	def factorial_(n: Int): Int = {
		def loop(acc: Int, n: Int): Int =
			if (n == 0) acc else loop(acc * n, n - 1)
		loop(1, n)
	}                                         //> factorial_ : (n: Int)Int
	
	factorial(4)                              //> res2: Int = 24
	factorial_(4)                             //> res3: Int = 24

}