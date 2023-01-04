sealed trait MyOption[+A] {
  /**
   * Returns a Some containing the result of applying f to this MyOption's value if this MyOption is nonempty.
   * Otherwise return None.
   */
  def map[B](f: A => B): MyOption[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }

  /**
   * Returns the result of applying f to this MyOption's value if this MyOption is nonempty.
   */
  def flatMap[B](f: A => MyOption[B]): MyOption[B] = this match {
    case None => None
    case Some(v) => f(v)
  }

  /**
   * Returns the result inside the Some case of the MyOption, or if the MyOption
   * is None, return the given default value.
   */
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }

  /**
   * Returns the first MyOption if it’s defined; otherwise, it returns the second MyOption
   */
  def orElse[B >: A](ob: => MyOption[B]): MyOption[B] = this match {
    case None => ob
    case _ => this
  }

  /**
   * Returns this MyOption if it is non-empty and applying the predicate f to this MyOption's value returns true.
   */
  def filter(f: A => Boolean): MyOption[A] = this match {
    case Some(v) if f(v) => this
    case _ => None
  }
}
case class Some[+A](get: A) extends MyOption[A]
case object None extends MyOption[Nothing]

object MyOption {
  /**
   * An MyOption factory which creates Some(x) if the argument is not null,
   * and None if it is null.
   */
  def apply[A](x: A): MyOption[A] = if (x == null) None else Some(x)

  /**
   * Returns the mean of a given Seq[Double]
   */
  private def mean(xs: Seq[Double]): MyOption[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  /**
   * Returns the variance of a given Seq[Double]
   */
  def variance(xs: Seq[Double]): MyOption[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  /**
   * Combines a MyList of MyOptions into one MyOption containing a list of all the Some values
   * in the original MyList. If the original MyList contains None even once,
   * the result of the function should be None;
   * otherwise the result should be Some with a MyList of all the values.
   */
  def sequence[A](as: MyList[MyOption[A]]): MyOption[MyList[A]] = as match {
    case Nil => Some(Nil)
    case Cons(h, t) => h.flatMap(elH => sequence(t).map(elH :: _))
  }

  /**
   * Combines two MyOption values using a binary function.
   * If either MyOption is None, then the return value is too
   */
  def map2[A, B, C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] =
    a.flatMap(elA => b.map(elB => f(elA, elB)))

  /**
   * Maps over a MyList using a function that might fail,
   * returning None if applying it to any element of the MyList returns None.
   */
  def traverse[A, B](a: MyList[A])(f: A => MyOption[B]): MyOption[MyList[B]] =
    a.foldRight[MyOption[MyList[B]]](Some(Nil))((aa, acc) => map2(f(aa), acc)(_ :: _))
}
