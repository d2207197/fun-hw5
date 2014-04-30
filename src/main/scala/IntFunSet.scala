package SetPractice


// import common._

/**
  * 2. Purely Functional Sets.
  */
object IntFunSets {
  type Set = Int => Boolean

  def contains(s: Set, elem: Int): Boolean = s(elem)
  def singletonSet(elem: Int): Set = (e: Int) => e == elem
  def union(s: Set, t: Set): Set = (e: Int) => s(e) || t(e)
  def intersect(s: Set, t: Set): Set = (e:Int) => s(e) && t(e)
  def diff(s: Set, t: Set): Set = (e:Int) => s(e) && ! t(e)
  def filter(s: Set, p: Int => Boolean): Set = (e:Int) => p(e) && s(e)
  val bound = 1000
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > 1000) true;
      else if (s(a) && (p(a) == false)) false;
      else iter(a + 1)
    }
    iter(-bound)
  }
  def exists(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > 1000) false
      else if (s(a) && p(a)) true
      else iter(a + 1)
    }
    iter(-bound)
  }
  def map(s: Set, f: Int => Int): Set = {
    def iter(a: Int): Set = {
      if (a > 1000) (e: Int) => false
      else if (s(a)) union(singletonSet(f(a)), iter(a + 1 ))
      else iter(a + 1)
    }
    iter(-bound)
  }
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }
  def printSet(s: Set) {
    println(toString(s))
  }
}



