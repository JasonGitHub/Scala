package week3

object intsets {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
}

abstract class IntSet {
	def incl(x: Int): IntSet
	def contains(x: Int): Boolean
	def union(other: IntSet): IntSet
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
	def incl(x: Int): IntSet = {
		if (x < elem) new NonEmpty(elem, left incl x, right)
		else if (x > elem) new NonEmpty(elem, left, right incl x)
		else this
	}
	def contains(x: Int): Boolean = {
		if (x < elem) left contains x
		else if (x > elem) right contains x
		else true
	}
	def union(other: IntSet): IntSet = {
		((left union right) union other) incl elem		// ???
	}
}

class Empty extends IntSet {
	def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)
	def contains(x: Int): Boolean = false
	def union(other: IntSet): IntSet = other
}

// Singleton
object Empty extends IntSet {
	def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
	def contains(x: Int): Boolean = false
	def union(other: IntSet): IntSet = other
}

abstract class Base {
	def foo = 1
	def bar: Int
}

class Sub extends Base {
	override def foo = 2
	def bar = 3
}