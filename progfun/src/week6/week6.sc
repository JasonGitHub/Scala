package week6

object week6 {
	// Combinations
	(1 to 3) flatMap (x => (4 to 6) map (y => (x, y)))
                                                  //> res0: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((1,4), (1,5)
                                                  //| , (1,6), (2,4), (2,5), (2,6), (3,4), (3,5), (3,6))
	// Scalar Product
	def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double = (xs zip ys).map{case (x, y) => x * y}.sum
                                                  //> scalarProduct: (xs: Vector[Double], ys: Vector[Double])Double
  // {case: p1 => e1 case: p2 => e2 ...} is equivalent to
  // x => x match {case: p1 => e1 case: p2 => e2 ...}
	scalarProduct(Vector(1, 2, 3), Vector(4, 5, 6))
                                                  //> res1: Double = 32.0
  def isPrime(n: Int): Boolean = (2 until n) forall (d => n % d != 0)
                                                  //> isPrime: (n: Int)Boolean
  isPrime(11)                                     //> res2: Boolean = true
}