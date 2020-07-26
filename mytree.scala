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

  def fold[A, B](t: Tree[A], z: B)(f1: (A, B) => B)(f2: (B, B) => B): B = {
    println(z)
    t match {
      case Leaf(a) => f1(a, z)
      case Branch(left, right) => f2(fold(left, z)(f1)(f2), fold(right, z)(f1)(f2))
    }
  }

  def sizeUsingFold[A](t: Tree[A]): Int = fold(t, 0)((_, i) => i + 1)(_ + _)

  def maxUsingFold(t: Tree[Int]): Int = fold(t, 0)(_ max _)(_ max _)

  def depthUsingFold[A](t: Tree[A]): Int = fold(t, 0)((_, i) => 1)(_ max _ + 1)

  def mapUsingFold[A](t: Tree[A])(f: A => B): Tree[B] = fold(t, Nil)((a, bs) => Leaf(f(a)))()
}
