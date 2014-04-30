package SetPractice

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

  case class AInt(value: Int) extends Eq[AInt] with HasOrder[AInt] {
    def eq2(other: AInt): Boolean = (value == other.value)
    def next = AInt(value + 1)
    def prev = AInt(value - 1)
    def GT: AInt => Boolean = (other) => (value > other.value)
    def LT: AInt => Boolean = (other) => (value < other.value)
  }

  type Set[A <: Eq[A] with HasOrder[A] ] = A => Boolean

  def empty[A <: Eq[A] with HasOrder[A] ]: Set[A] = (e: A) => false
  def contains[A <: Eq[A] with HasOrder[A] ](s: Set[A], elem: A): Boolean = s(elem)
  def singletonSet[A <: Eq[A] with HasOrder[A] ](elem: A): Set[A] = (e: A) => e eq2 elem
  def union[A <: Eq[A] with HasOrder[A] ](s: Set[A], t: Set[A]): Set[A] = (e: A) => s(e) || t(e)
  def intersect[A <: Eq[A] with HasOrder[A] ](s: Set[A], t: Set[A]): Set[A] = (e:A) => s(e) && t(e)
  def diff[A <: Eq[A] with HasOrder[A] ](s: Set[A], t: Set[A]): Set[A] = (e:A) => s(e) && ! t(e)
  def filter[A <: Eq[A] with HasOrder[A] ](s: Set[A], p: A => Boolean): Set[A] = (e:A) => p(e) && s(e)

  def unfold[A <: Eq[A] with HasOrder[A], B](z: B)(pterminal: B => Boolean)(gen: B => (B,A)): Set[A] = {
    @annotation.tailrec
    def iter(z: B, s: Set[A]): Set[A] = 
      if (pterminal(z)) s
      else {
        val (b, a) = gen(z)
        iter(b, union(singletonSet(a), s))
      }
    iter(z, empty)
  }



  case class BoundedMethods[A <: Eq[A] with HasOrder[A]](min: A, max: A) {
    def fromList(l: List[A]): Set[A] = {
      val lt = l.toIterator
      unfold(lt)( ! _.hasNext)( x => ( x ,x.next) )
    }

    def filter(s: Set[A], p: A => Boolean): Set[A] =
      foldDown(s)(empty: Set[A]){ (a, z) =>
        if (p(a)) union(singletonSet(a), z) else z
      }

    def forall(s: Set[A], p: A => Boolean): Boolean = 
      foldDown(s)(true){ (a,z) =>
        if (z == false) false else p(a)
      }

    def exists(s: Set[A], p: A => Boolean): Boolean = 
      foldDown(s)(false){ (a, z) =>
        if (z == true) true else p(a)
      }

    def map(s: Set[A], f: A => A): Set[A] = 
      foldDown(s)(empty: Set[A]){ (a, z) =>
        union(singletonSet(f(a)), z)
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

}

