package week1

object session {
  def general(f: Int => Int)(g: (Int, Int) => Int, base: Int)(a: Int, b: Int): Int = {
		if (a > b) base
		else g(f(a), general(f)(g, base)(a + 1, b))
  }                                               //> general: (f: Int => Int)(g: (Int, Int) => Int, base: Int)(a: Int, b: Int)Int
                                                  //| 
 	general(x => x)((x, y) => x * y, 1)(1, 4) //> res0: Int = 24
 	general(x => x)((x, y) => x + y, 0)(1, 4) //> res1: Int = 10
}