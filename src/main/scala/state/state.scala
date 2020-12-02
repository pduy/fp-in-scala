package fpinscala.state

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5deece66dL + 0xbL) & 0xfffffffffffL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}


case class State[S, +A](run: S => (A, S)) {
  def map[B](s: S[A])(f: A => B): State[S, B] = State{
    s => 
      val (a, s1) = run(s)
      (s1, f(a))
  }

  def flatMap[A, B](g: A => State[S, B]): State[S, B] = State{
    s =>
      val (a, s1) = run(s)
      g(a).run(s1)
  }
}


object State {
  type Rand[+A] = RNG => (A, RNG)
  type State[S, +A] = S => (A, S)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, nextRNG) = rng.nextInt
    val nonNegInt = i match {
      case Int.MinValue => Int.MaxValue
      case _            => if (i > 0) i else -i
    }

    (nonNegInt, nextRNG)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, nextRNG) = nonNegativeInt(rng)
    ((Int.MaxValue - i).toDouble / Int.MaxValue, nextRNG)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, nextRNG) = rng.nextInt
    val (d, nextDRNG) = double(nextRNG)
    ((i, d), nextDRNG)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val xs = (1 to count)
      .foldRight(List(rng.nextInt))((_, acc) =>
        acc ++ List(acc.last._2.nextInt)
      )

    val generatedInts = xs.map(_._1)
    (generatedInts, xs.last._2)
  }

  def unit[S, A](a: A): State[S, A] =
    s => (a, s)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  //def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
  //rng => {
    //val (a, arng) = ra(rng)
    //val (b, brng) = rb(arng)
    //(f(a, b), brng)
  //}

  def map2[A, B, C](sa: State[S, A], sb: State[S, B])(f: (A, B) => C): State[S, C] =
    s => {
      val (a, s1) = sa.run()
      val (b, s2) = sa.run()
      (f(a, b), s2)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight[Rand[List[A]]](unit(List()))((rng: Rand[A], rands: Rand[List[A]]) => map2(rng, rands)(_ :: _))

  def intsUsingSequence(count: Int): Rand[List[Int]] = sequence(List.fill(count)(unit(0)))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => map(f)(g)(rng)
}
