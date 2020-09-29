package fpinscala.lazyness


sealed trait Stream[+A] {
  import Stream._

  def headOption: Option[A] = this match {
    case Cons(h, t) => Some(h())
    case _ => None
  }

  def toList: List[A] = {
    def traverse(as: Stream[A]): List[A] = as match {
      case Cons(h, t) => h() :: traverse(t())
      case _ => List()
    }
    traverse(this)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) => if (n > 0) Cons(h, () => t().take(n - 1)) else Empty
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) => if (n > 0) t().drop(n - 1) else Cons(h, t)
    case _ => Empty
  }

  //def takeWhile(p: A => Boolean): Stream[A] = this match {
    //case Cons(h, t) => if (p(h())) Cons(h, () => t().takeWhile(p)) else Empty
    //case _ => Empty
  //}

  def foldRight[B](z: => B)(f: (A, => B) => B): B = 
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def takeWhile(p: A => Boolean): Stream[A] = 
    foldRight(empty[A])((a, b) => if(p(a)) cons(a, b) else Empty)

  def map[B](f: A => B): Stream[B] = 
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] = 
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
