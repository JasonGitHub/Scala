package week7

object prime {
	def from(n: Int): Stream[Int] = n #:: from(n + 1)
                                                  //> from: (n: Int)Stream[Int]
	
  def sieve(ns: Stream[Int]): Stream[Int] = {
  	ns.head #:: sieve(ns.tail filter (_ % ns. head != 0))
  }                                               //> sieve: (ns: Stream[Int])Stream[Int]

	val prime = sieve(from(2))                //> prime  : Stream[Int] = Stream(2, ?)
	prime.take(3).toList                      //> res0: List[Int] = List(2, 3, 5)
}