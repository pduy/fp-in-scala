package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => size(left) + size(right)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => (depth(left) max depth(right)) + 1
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A, B](t: Tree[A])(f1: A => B)(f2: (B, B) => B): B = {
    t match {
      case Leaf(a) => f1(a)
      case Branch(left, right) => f2(fold(left)(f1)(f2), fold(right)(f1)(f2))
    }
  }

  def sizeUsingFold[A](t: Tree[A]): Int = fold(t)(a => 1)(_ + _)

  def maxUsingFold(t: Tree[Int]): Int = fold(t)(i => i)(_ max _)

  def depthUsingFold[A](t: Tree[A]): Int = fold(t)(a => 0)((d1, d2) => 1 + (d1 max d2))

  def mapUsingFold[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
}
