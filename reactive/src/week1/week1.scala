package week1

class week1 {
  trait Generator[+T] {
    self =>
    def generate: T
    def map[S](f: T => S): Generator[S] = new Generator[S] {
      def generate = f(self.generate)
    }
    def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
      def generate = f(self.generate).generate
    }
  }
  // cumbersome 
  val integers = new Generator[Int] {
    def generate = scala.util.Random.nextInt()
  }
//  val booleans = new Generator[Boolean] {
//    def generate = integers.generate > 0
//  }
//  val pairs = new Generator[(Int, Int)] {
//    def generate = (integers.generate, integers.generate)
//  }
  
  // clearer
  val booleans = for (x <- integers) yield x > 0
  // val booleans = integers map (x => x > 0)
  
  def pairs[T, U](t: Generator[T], u: Generator[U]) = for {
    x <- t
    y <- u
  } yield (x, y)
  // def pairs[T, U](t: Generator[T], u: Generator[U]) = t flatMap (x => u map (y => (x, y)))
  
  def single[T](x: T): Generator[T] = new Generator[T] {
    def generate = x
  }
  
  def choose(lo: Int, hi: Int): Generator[Int] = 
    for (x <- integers) yield lo + x % (hi - lo)
  
  def oneOf[T](xs: T*): Generator[T] = 
    for (idx <- choose(0, xs.length)) yield xs(idx)
  
  // List
  def lists: Generator[List[Int]] =
    for {
      isEmpty <- booleans
      list <- if (isEmpty) emptyLists else nonEmptyLists
    } yield list
    
  def emptyLists = single(Nil)
  
  def nonEmptyLists = for {
    head <- integers
    tail <- lists
  } yield head :: tail
  
  // Tree
  trait Tree
  case class Inner(left: Tree, right: Tree) extends Tree
  case class Leaf(x: Int) extends Tree
  
  def leafs: Generator[Leaf] = for {
    x <- integers
  } yield Leaf(x)
  
  def inners: Generator[Inner] = for {
    l <- trees
    r <- trees
  } yield Inner(l, r)
  
  def trees: Generator[Tree] = for {
    isLeaf <- booleans
    tree <- if (isLeaf) leafs else inners
  } yield tree
  
  // Random Testing
  def test[T](g: Generator[T], numTimes: Int = 100)
  	(test: T => Boolean): Unit = {
    for (i <- 0 until numTimes) {
      val value = g.generate
      assert(test(value), "test failed for " + value)
    }
    println("passed " + numTimes + " tests")
  }
  // example use of random testing
  test(pairs(lists, lists)) {
    case (xs, ys) => (xs ++ ys).length > xs.length
  }
  
  // Monad
  // a monad M is a parametric type M[T] with two operations, flatMap and unit, that have to satisfy some laws
  trait M[T] {
    def faltMap[U](f: T => M[U]): M[U]
  }
  
  def unit[T](x: T): M[T] = ???
}