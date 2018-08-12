package training

object Chapter4 {

  sealed trait Option[+A] {

    /**
      * Exercise 4.1
      * Implement basic Option functions.
      */
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(a) => Some(f(a))
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(a) => a
    }

    def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

    def flatMap_1[B](f: A => Option[B]): Option[B] = this match {
      case None => None
      case Some(a) => f(a)
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = this map (Some(_)) getOrElse ob

    def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)

    def filter_1(f: A => Boolean): Option[A] = this match {
      case Some(a) if f(a) => this
      case _ => None
    }

  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  /**
    * Exercise 4.2
    * Implement variance as defined <a href="https://en.wikipedia.org/wiki/Variance#Definition">here</a>.
    */
  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  case class Some[+A](get: A) extends Option[A]

  case object None extends Option[Nothing]

  def main(args: Array[String]): Unit = {

    println("Option Map: " + Some(2).map(i => i * 2))

    println("Option GetOrElse: " + Some(2).getOrElse(3))

    println("Option FlatMap: " + Some(2).flatMap(i => Some(i * 3)))

    println("Option OrElse: " + None.orElse(Some(3)))

    println("Option Filter: ")
  }

}
