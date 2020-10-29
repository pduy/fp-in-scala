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

object State {
  type Rand[+A] = RNG => (A, RNG)

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
}
