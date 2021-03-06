package fpinscala.lazyness


sealed trait Stream[+A] {
  import Stream._

  def headOption: Option[A] = this match {
    case Cons(h, t) => Some(h())
    case _ => None
  }

  def toList: List[A] = {
    this match {
      case Cons(h, t) => h() :: t().toList
      case _ => List()
    }
  }

  //def take(n: Int): Stream[A] = this match {
    //case Cons(h, t) =>
      //if (n > 0) cons(h(), t().take(n - 1)) else Empty
    //case _ => Empty
  //}

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) => if (n > 0) t().drop(n - 1) else Cons(h, t)
    case _ => Empty
  }

  //def takeWhile(p: A => Boolean): Stream[A] = this match {
    //case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    //case _ => Empty
  //}

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  //def takeWhile(p: A => Boolean): Stream[A] =
    //foldRight(empty[A])((a, b) => if(p(a)) cons(a, b) else Empty)

  //def map[B](f: A => B): Stream[B] =
    //foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  def append[B >: A](elem: => Stream[B]): Stream[B] =
    foldRight(elem)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a) append b)

  def map[B](f: A => B): Stream[B] =
    unfold(this)(stream => stream.headOption match {
      case None => None
      case Some(a) => Some(f(a), stream.drop(1))
    })

  def take(n: Int): Stream[A] =
    unfold((this, n)){
      case (Cons(h, t), 1) => Some((h(), (empty, 0)))
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
      case _ => None
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    unfold(this){
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWith[B](s2: Stream[B]): Stream[(A, B)] =
    unfold((this, s2)){
      case (Empty, _) => None
      case (_, Empty) => None
      case (stream1, stream2) =>
        Some((stream1.headOption.get, stream2.headOption.get), (stream1.drop(1), stream2.drop(1)))
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)){
      case (Empty, Empty) => None
      case (stream1, stream2) =>
        Some((stream1.headOption, stream2.headOption), (stream1.drop(1), stream2.drop(1)))
    }

  def startsWith[A](s: Stream[A]): Boolean = zipAll(s).foldRight(true){
    case ((Some(a), Some(b)), flag) => a == b && flag
    case ((None, Some(b)), _) => false
    case ((Some(a), None), flag) => flag
  }

  def tails: Stream[Stream[A]] = unfold(this){
    case Cons(h, t) => Some((Cons(h, t), t()))
    case _ => Some((empty[A]), empty[A])
  }

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = this match {
    case Cons(h, t) => t().scanRight(z)(f).map(b => f(h(), b))
    case _ => empty[B]
  }
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

  //def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def constant[A](a: A): Stream[A] = {
    lazy val x: Stream[A] = cons(a, x)
    x
  }

  //def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  //def fibs(): Stream[Int] = {
    //def f(n1: Int, n2: Int): Stream[Int] = cons(n1, f(n2, n1 + n2))

    //f(0, 1)
  //}

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => empty[A]
  }

  //def constant[A](a: A): Stream[A] = unfold(142)(_ => Some((a, 142)))

  def ones = constant(1)

  def from(n: Int): Stream[Int] = unfold(n)(s => Some((s, s + 1)))

  def fibs(): Stream[Int] = unfold((0, 1)){
    case (a, s) => Some(a + s, (s, a + s))
  }
}
