package funsets


/**
  * 2. Purely Functional Sets.
  */
object FunSets {
  /**
    * We represent a set by its characteristic function, i.e.
    * its `contains` predicate.
    */
  type Set = Int => Boolean

  /**
    * Indicates whether a set contains a given element.
    */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
    * Returns the set of the one given element.
    */
  def singletonSet(elem: Int): Set = {
    new Set {
      override def apply(v1: Int): Boolean = v1 == elem
    }
  }

  /**
    * Returns the union of the two given sets,
    * the sets of all elements that are in either `s` or `t`.
    * inspired from http://docs.scala-lang.org/overviews/collections/sets.html
    **/
  def union(s: Set, t: Set): Set = (x: Int) => s(x) || t(x)

  /**
    * Returns the intersection of the two given sets,
    * the set of all elements that are both in `s` and `t`.
    * inspired from http://docs.scala-lang.org/overviews/collections/sets.html
    */
  def intersect(s: Set, t: Set): Set = (x: Int) => s(x) && t(x)

  /**
    * Returns the difference of the two given sets,
    * the set of all elements of `s` that are not in `t`.
    */
  def diff(s: Set, t: Set): Set = (x: Int) => s(x) && !t(x)

  /**
    * Returns the subset of `s` for which `p` holds.
    * filter - filter which takes a predicate function as param and return a subset witch matches the predicate
    */

  //use with new set
  /*  def filter(s: Set, p: Int => Boolean): Set = {
      new Set {
        override def apply(v1: Int): Boolean = s(v1) & p(v1)  //contains(s, v1) && p(v1)
      }
    }*/

  //or user with evaluation
  def filter(s: Set, p: Int => Boolean): Set = (x: Int) => s(x) && p(x)

  /**
    * The bounds for `forall` and `exists` are +/- 1000.
    */
  val bound = 1000

  /**
    * Returns whether all bounded integers within `s` satisfy `p`.
    */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    @scala.annotation.tailrec
    def iter(v1: Int): Boolean = {
      if (v1 > bound) true
      else if (s(v1) & !p(v1)) false //or if(contains(s, v1) && !p(v1)) false
      else iter(v1 + 1) //next elem
    }
    iter(-bound)
  }

  /**
    * Returns whether there exists a bounded integer within `s`
    * that satisfies `p`.
    */
  def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, (i: Int) => !p(i))

  /**
    * Returns a set transformed by applying `f` to each element of `s`.
    */
  def map(s: Set, f: Int => Int): Set = (y: Int) => exists(s, x => f(x) == y)

  /**
    * Displays the contents of a set
    */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
    * Prints the contents of a set on the console.
    */
  def printSet(s: Set) {
    println(toString(s))
  }
}
