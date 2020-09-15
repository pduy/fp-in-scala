package fpinscala.errorhandling


sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def orElse[B >: A](ob: => Option[B]): Option[B] =  this match {
    case None => ob
    case Some(a) => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case None => None
    case Some(a) => if (f(a)) this else None
  }
}

case object None extends Option[Nothing]

case class Some[+A](get: A) extends Option[A] 


object Option {
  def mean(xs: Seq[Double]): Option[Double] = 
    if ( xs.isEmpty ) None
    else Some(xs.sum / xs.size)

  def variance(xs: Seq[Double]): Option[Double] = 
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  val abs0: Option[Double] => Option[Double] = lift(math.abs)

  //def insuranceRateQuote(age: String, numberOfSpeedingTickets: String): Double = {
    //val optAge: Option[Int] = Try(age.toInt)
    //val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
  //}

  def Try[A](a: => A): Option[A] = {
    println("a")
    try Some(a)
    catch {case e: Exception => None}
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = 
    a.flatMap(a_ => b.map(b_ => f(a_, b_)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = 
    a.foldRight[Option[List[A]]](Some(Nil))((optA, optListA) => map2(optA, optListA)(_ :: _))

  // Not optimal implementation
  def parseInts(a: List[String]): Option[List[Int]] =
    sequence(a map (i => Try(i.toInt)))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = 
    a.foldRight[Option[List[B]]](Some(Nil))((aa, optListB) => map2(f(aa), optListB)(_ :: _))
}
