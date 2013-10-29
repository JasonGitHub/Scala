package week5

import math.Ordering

object week5 {
  val pair = ("answer", 42)                       //> pair  : (String, Int) = (answer,42)
  val (label, value) = pair                       //> label  : String = answer
                                                  //| value  : Int = 42
  label                                           //> res0: String = answer
  value                                           //> res1: Int = 42
  def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
        	if (ord.lt(x, y)) x :: merge(xs1, ys)
        	else y :: merge(xs, ys1)
      }
      val (fst, snd) = xs splitAt n
      merge(msort(fst), msort(snd))
    }
  }                                               //> msort: [T](xs: List[T])(implicit ord: scala.math.Ordering[T])List[T]

  val nums = List(2, 3, -1, 5, 4, 0)              //> nums  : List[Int] = List(2, 3, -1, 5, 4, 0)
  val fruits = List("apple", "pineapple", "banana")
                                                  //> fruits  : List[String] = List(apple, pineapple, banana)
  msort(nums)                                     //> res2: List[Int] = List(-1, 0, 2, 3, 4, 5)
  msort(fruits)                                   //> res3: List[String] = List(apple, banana, pineapple)
  
  def squareList(xs: List[Int]): List[Int] = xs match {
  	case Nil => xs
  	case y :: ys => y * y :: squareList(ys)
  }                                               //> squareList: (xs: List[Int])List[Int]
  squareList(nums)                                //> res4: List[Int] = List(4, 9, 1, 25, 16, 0)
  nums map (x => x * x)                           //> res5: List[Int] = List(4, 9, 1, 25, 16, 0)
  
  def posElems(xs: List[Int]): List[Int] = xs match {
  	case Nil => xs
  	case y :: ys => if (y > 0) y :: posElems(ys) else posElems(ys)
  }                                               //> posElems: (xs: List[Int])List[Int]
  posElems(nums)                                  //> res6: List[Int] = List(2, 3, 5, 4)
  nums filter (x => x > 0)                        //> res7: List[Int] = List(2, 3, 5, 4)
  nums filterNot (x => x > 0)                     //> res8: List[Int] = List(-1, 0)
  nums partition (x => x > 0)                     //> res9: (List[Int], List[Int]) = (List(2, 3, 5, 4),List(-1, 0))
  nums takeWhile (x => x > 0)                     //> res10: List[Int] = List(2, 3)
  nums dropWhile (x => x > 0)                     //> res11: List[Int] = List(-1, 5, 4, 0)
  nums span (x => x > 0)                          //> res12: (List[Int], List[Int]) = (List(2, 3),List(-1, 5, 4, 0))
  
  def pack[T](xs: List[T]): List[List[T]] = xs match {
  	case Nil => Nil
  	case x :: xs1 =>
  		val (first, rest) = xs span (y => y == x)
  		first :: pack(rest)
  }                                               //> pack: [T](xs: List[T])List[List[T]]
  
  val data = List("a", "a", "b", "c")             //> data  : List[String] = List(a, a, b, c)
  pack(data)                                      //> res13: List[List[String]] = List(List(a, a), List(b), List(c))
  
  def encode[T](xs: List[T]): List[(T, Int)] =
  	pack(xs) map (ys => (ys.head, ys.length)) //> encode: [T](xs: List[T])List[(T, Int)]
  encode(data)                                    //> res14: List[(String, Int)] = List((a,2), (b,1), (c,1))
  
  def sum(xs: List[Int]) = (0 :: xs) reduceLeft ((x, y) => x + y)
                                                  //> sum: (xs: List[Int])Int
  def product(xs: List[Int]) = (1 :: xs) reduceLeft (_ * _)
                                                  //> product: (xs: List[Int])Int
  
  def sum1(xs: List[Int]) = (xs foldLeft 0)(_ + _)//> sum1: (xs: List[Int])Int
  def product1(xs: List[Int]) = (xs foldLeft 1)(_ * _)
                                                  //> product1: (xs: List[Int])Int
}