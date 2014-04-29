package SetPractice


// import common._

/**
  * 2. Purely Functional Sets.
  */
object IntFunSets {
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
  def singletonSet(elem: Int): Set = (e: Int) => e == elem

  /**
    * Returns the union of the two given sets,
    * the sets of all elements that are in either `s` or `t`.
    */
  def union(s: Set, t: Set): Set = (e: Int) => s(e) || t(e)

  /**
    * Returns the intersection of the two given sets,
    * the set of all elements that are both in `s` and `t`.
    */
  def intersect(s: Set, t: Set): Set = (e:Int) => s(e) && t(e)

  /**
    * Returns the difference of the two given sets,
    * the set of all elements of `s` that are not in `t`.
    */
  def diff(s: Set, t: Set): Set = (e:Int) => s(e) && ! t(e)

  /**
    * Returns the subset of `s` for which `p` holds.
    */
  def filter(s: Set, p: Int => Boolean): Set = (e:Int) => p(e) && s(e)

  /**
    * The bounds for `forall` and `exists` are +/- 1000.
    */
  val bound = 1000

  /**
    * Returns whether all bounded integers within `s` satisfy `p`.
    */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > 1000) true;
      else if (s(a) && (p(a) == false)) false;
      else iter(a + 1)
    }
    iter(-bound)
  }
  
  /**
    * Returns whether there exists a bounded integer within `s`
    * that satisfies `p`.
    */
  def exists(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > 1000) false
      else if (s(a) && p(a)) true
      else iter(a + 1)
    }
    iter(-bound)
  }

  /**
    * Returns a set transformed by applying `f` to each element of `s`.
    */
  def map(s: Set, f: Int => Int): Set = {
    def iter(a: Int): Set = {
      if (a > 1000) (e: Int) => false
      else if (s(a)) union(singletonSet(f(a)), iter(a + 1 ))
      else iter(a + 1)
    }
    iter(-bound)
  }

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

object GeneralFunSet {

  trait Eq[A]{
    def eq2(other: A): Boolean
  }

  trait HasOrder[A] {
    def next: A
    def prev: A
    def GT: A => Boolean
    def LT: A => Boolean

  }


  trait TraficLight extends Eq[TraficLight]
  case object Red extends TraficLight {
    def eq2(t: TraficLight): Boolean = t match {
      case Red =>  true
      case Green => false
    }
  }

  case object Green extends TraficLight {
    def eq2(t: TraficLight): Boolean = t match {
      case Red => true
      case Green => false
    }
  }


  type Set[A <: Eq[A] with HasOrder[A] ] = A => Boolean


  def empty[A <: Eq[A] with HasOrder[A] ]: Set[A] = (e: A) => false

  def contains[A <: Eq[A] with HasOrder[A] ](s: Set[A], elem: A): Boolean = s(elem)

  def singletonSet[A <: Eq[A] with HasOrder[A] ](elem: A): Set[A] = (e: A) => e eq2 elem

  def union[A <: Eq[A] with HasOrder[A] ](s: Set[A], t: Set[A]): Set[A] = (e: A) => s(e) || t(e)
  
  def intersect[A <: Eq[A] with HasOrder[A] ](s: Set[A], t: Set[A]): Set[A] = (e:A) => s(e) && t(e)

  def diff[A <: Eq[A] with HasOrder[A] ](s: Set[A], t: Set[A]): Set[A] = (e:A) => s(e) && ! t(e)

  def filter[A <: Eq[A] with HasOrder[A] ](s: Set[A], p: A => Boolean): Set[A] = (e:A) => p(e) && s(e)

  // val bound = 1000

  case class BoundedMethods[A <: Eq[A] with HasOrder[A]](min: A, max: A) {


    // def forall(s: Set[A], p: A => Boolean): Boolean = {
    //   def iter(a: A): Boolean = {
    //     // println(s"forall.iter($a)")
    //     // Thread sleep 1000
    //     if (a GT max) true;
    //     else if (s(a) && (p(a) == false)) false;
    //     else iter(a.next)
    //   }
    //   iter(min)
    // }

    def forall(s: Set[A], p: A => Boolean): Boolean = {
      foldDown(s)(true){ (a,z) =>
        if (z == false) false
        else if (a GT max) z
        else p(a)
      }
    }



    def exists(s: Set[A], p: A => Boolean): Boolean = {
      def iter(a: A): Boolean = {
        if ( a GT max) false
        else if (s(a) && p(a) == true) true
        else iter(a.next)
      }
      iter(min)
    }

    def map(s: Set[A], f: A => A): Set[A] = {
      def iter(a: A): Set[A] = {
        if (a GT max) empty
        else if (s(a)) union(singletonSet(f(a)), iter(a.next))
        else iter(a.next)
      }
      iter(min)
    }


    def toList(s: Set[A]):List[A] = {
      def iter(a: A): List[A] = {
        if (a GT max) Nil
        else if (s(a)) a :: iter(a.next) 
        else iter(a.next)
      }
      iter(min)
    }


    def toString(s: Set[A]): String = {
      // val xs = while {min} (i <- min to bound if contains(s, i)) yield i
      toList(s).mkString("<", ", ", ">")
    }

    def printSet(s: Set[A]) {
      println(toString(s))
    }

    def foldDown[B](s: Set[A])(z: B)(f: (A,B) => B): B = {
      toList(s).:\(z)(f)
    }
  }

  case class AInt(value: Int) extends Eq[AInt] with HasOrder[AInt] {
    def eq2(other: AInt): Boolean = (value == other.value)
    def next = AInt(value + 1)
    def prev = AInt(value - 1)
    def GT: AInt => Boolean = (other) => (value > other.value)
    def LT: AInt => Boolean = (other) => (value < other.value)
  }

  val s0 = singletonSet(AInt(0))
  val s1 = singletonSet(AInt(1))
  val s2 = singletonSet(AInt(2))
  val s3 = singletonSet(AInt(3))
  val s4 = singletonSet(AInt(4))
  val s5 = singletonSet(AInt(5))

  val s01 = union(s0, s1)
  val s12 = union(s1, s2)
  val s23 = union(s2, s3)
  val s34 = union(s3, s4)
  val s45 = union(s4, s5)

  val s012 = union(s01, s2)
  val s123 = union(s12, s3)
  val s234 = union(s23, s4)
  val s345 = union(s34, s5)

  val bounded = BoundedMethods(AInt(0), AInt(4))
}


object Main extends App {
  import IntFunSets._
  
  val x = union(singletonSet(1), singletonSet(2))
  printSet(x)
  printSet(map(x, (a:Int)=>a+1))

  println(forall(x, (a:Int)=> a>1))
  
  val y = intersect(singletonSet(1), singletonSet(2))
  exists(y, (a:Int)=> a<3)
}


