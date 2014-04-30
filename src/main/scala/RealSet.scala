package SetPractice

// FUN Programming Homework 5
// 13.

// immutable covariant List-like Set
// field prevInstance keeps previous Set instance, like tail in List

class RealSet[+A] {
  protected def prevInstance: RealSet[A] = null
  protected def elem: A = sys.error("no element")

  def apply[T >: A](e: T): Boolean = false // contains
  def contains[T >: A](e: T) = apply(e)

  def $plus[T >: A](e: T): RealSet[T] = new RealSet[T] {
    override def prevInstance: RealSet[T] = RealSet.this.asInstanceOf[RealSet[A]]
    override def elem: T = e
    override def apply[T](elm: T): Boolean = {
      elm == elem || prevInstance.apply(elm)
    }
  }

  def $minus[T >: A](e: T): RealSet[T] = 
    if (this.apply(e)) {
      if (elem == e) prevInstance
      else prevInstance - e + elem
    }
    else
      RealSet.this

  def union[T >: A](that: RealSet[T]): RealSet[T] =
    if (that.prevInstance == null) this
    else {
      if (this(that.elem))
        this union that.prevInstance
      else
        (this + that.elem) union that.prevInstance
    }

  def foldDown[T >: A, B](s: B)(f: (T, B) => B): B =
    if (this.prevInstance == null) s
    else 
      this.prevInstance.foldDown(f(this.elem, s))(f)

  def filter[T >: A](p: T => Boolean): RealSet[T] =
    this.foldDown(new RealSet[T]){ (e, s) =>
      if (p(e)) s + e
      else s
    }

  def diff[T >: A](that: RealSet[T]): RealSet[T] =
    this.filter(!that(_))
 
  def intersect[T >: A](that: RealSet[T]): RealSet[T] =
    this.filter(that(_))

  def exists[T >: A](p: T => Boolean): Boolean =
    foldDown(false){ (e, s) =>
      if (s == true) true else p(e)
    }
  def forall[T >: A](p: T => Boolean): Boolean =
    !exists(!p(_))

  def map[T >: A, B](f: T => B): RealSet[B] =
    foldDown(new RealSet[B]){ (e, s) =>
      println(e, f(e), s)
      s + f(e)
    }

  def toList: List[A] = {
    if (this.prevInstance != null)
      this.elem :: this.prevInstance.toList
    else Nil
  }

  override def toString: String = {
    toList.mkString("<", ", ", ">")
  }
  // def unfoldRight[A, B](seed: B)(f: B => Option[(A, B)]): 
}

