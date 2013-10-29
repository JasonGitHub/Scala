package week5

class lists {
  def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error("init of empty list")
    case List(x) => List()
    case y :: ys => y :: init(ys)  
  }
  
  def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
    case List() => ys
    case z :: zs => z :: concat(zs, ys)
  }
  
  def concat1[T](xs: List[T], ys: List[T]): List[T] = (xs foldRight ys)(_ :: _)
  
  def reverse[T](xs: List[T]): List[T] = xs match {
    case List() => xs
    case y :: ys => reverse(ys) ++ List(y)
  }
  
  def removeAt[T](xs: List[T], n: Int): List[T] = (xs take n) ++ (xs drop n + 1)
  
  
}