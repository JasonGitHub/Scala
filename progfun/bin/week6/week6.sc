package week6

object week6 {
  // Combinations
  (1 to 3) flatMap (x => (4 to 6) map (y => (x, y)))
                                                  //> res0: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((1,4), (1,5
                                                  //| ), (1,6), (2,4), (2,5), (2,6), (3,4), (3,5), (3,6))
  // Scalar Product
  def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double = (xs zip ys).map { case (x, y) => x * y }.sum
                                                  //> scalarProduct: (xs: Vector[Double], ys: Vector[Double])Double
  // {case: p1 => e1 case: p2 => e2 ...} is equivalent to
  // x => x match {case: p1 => e1 case: p2 => e2 ...}
  scalarProduct(Vector(1, 2, 3), Vector(4, 5, 6)) //> res1: Double = 32.0
  def isPrime(n: Int): Boolean = (2 until n) forall (d => n % d != 0)
                                                  //> isPrime: (n: Int)Boolean
  isPrime(13)                                     //> res2: Boolean = true

  def findPrimePairs(n: Int) =
    for {
      i <- 1 to n
      j <- 1 to i
      if isPrime(i + j)
    } yield (i, j)                                //> findPrimePairs: (n: Int)scala.collection.immutable.IndexedSeq[(Int, Int)]

  findPrimePairs(10)                              //> res3: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((1,1), (2,1
                                                  //| ), (3,2), (4,1), (4,3), (5,2), (6,1), (6,5), (7,4), (7,6), (8,3), (8,5), (9,
                                                  //| 2), (9,4), (9,8), (10,1), (10,3), (10,7), (10,9))

  def scalarProduct1(xs: List[Double], ys: List[Double]): Double = {
    (for ((x, y) <- xs zip ys) yield (x * y)).sum
  }                                               //> scalarProduct1: (xs: List[Double], ys: List[Double])Double
  scalarProduct1(List(1, 2, 3), List(4, 5, 6))    //> res4: Double = 32.0
  
  
}