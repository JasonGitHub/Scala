package hacker_rank

object intro {
  def f(coefficients: List[Int], powers: List[Int], x: Double): Double =
    (coefficients.view zip powers map (e => e._1 * math.pow(x, e._2))).sum
                                                  //> f: (coefficients: List[Int], powers: List[Int], x: Double)Double

  def area(coefficients: List[Int], powers: List[Int], x: Double): Double =
    math.Pi * math.pow(f(coefficients, powers, x), 2)
                                                  //> area: (coefficients: List[Int], powers: List[Int], x: Double)Double

  def summation(
    func: (List[Int], List[Int], Double) => Double,
    upperLimit: Int,
    lowerLimit: Int,
    c: List[Int],
    p: List[Int]): Double =
    (lowerLimit.toDouble to upperLimit.toDouble by 0.001) map (func(c, p, _) * 0.001) sum
                                                  //> summation: (func: (List[Int], List[Int], Double) => Double, upperLimit: Int,
                                                  //|  lowerLimit: Int, c: List[Int], p: List[Int])Double
}