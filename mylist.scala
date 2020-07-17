package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]
case class NonEmptyList[+A](head: A, tail: List[A])

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = 
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => List()
    case Cons(x, xs) => xs
  }

  def setHead[A](head: A, xs: List[A]) = xs match {
    case Nil => Nil
    case Cons(x, xs) => Cons(head, xs)
  }

  @scala.annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
    case (Nil, _) => Nil
    case (l, 0) => l
    case (Cons(x, xs), n) => drop(xs, n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else Cons(x, dropWhile(xs, f))
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def append[A](l1: List[A], l2: List[A]): List[A] = l1 match {
    case Nil => l2
    case Cons(x, xs) => Cons(x, append(xs, l2)) }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    println(z)
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  def length[A](as: List[A]): Int = foldRight(as, 0)((_, x) => x + 1)

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    println(z)
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  def reverse[A](as: List[A]): List[A] = 
    foldLeft[A, List[A]](as, List())((xs, a) => Cons(a, xs))

  def foldLeftFromFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = 
    foldRight(as, z)((a, b) => f(b, a))

  def foldRightFromFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(as, z)((b, a) => f(a, b))

  def appendUsingFold[A](l1: List[A], l2: List[A]): List[A] = 
    foldRight[A, List[A]](l1, List())((a, xs) => Cons(a, xs))

  def map[A, B](as: List[A])(f: A => B): List[B] = as match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) => if(f(x)) Cons(x, filter(xs)(f)) else filter(xs)(f)
  }
    
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil => Nil
    case Cons(x, xs) => append(f(x), flatMap(xs)(f))
  }

  def elementWiseSum(l1: List[Int], l2: List[Int]): List[Int] = l1 match {
    case Nil => Nil
    case Cons(x1, xs1) => l2 match {
      case Nil => Nil
      case Cons(x2, xs2) => Cons(x1 + x2, elementWiseSum(xs1, xs2))
    }
  }

  def zipWith[A, B](l1: List[A], l2: List[A])(f: (A, A) => B): List[B] = (l1, l2) match {
    case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(f(x1, x2), zipWith(xs1, xs2))
    case _ => Nil
  }
}
