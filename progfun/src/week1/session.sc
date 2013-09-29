package week1

object session {
  def abs(x: Double): Double = if (x >= 0) x else -x
                                                  //> abs: (x: Double)Double
  
  def sqrt(x: Double): Double = {
    
    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double): Boolean =
      abs(guess * guess - x) / x < 1e-10

    def improve(guess: Double): Double =
      (guess + x / guess) / 2

    sqrtIter(1.0)
  }                                               //> sqrt: (x: Double)Double
	
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
	
	factorial(4)                              //> res0: Int = 24
	factorial_(4)                             //> res1: Int = 24

}