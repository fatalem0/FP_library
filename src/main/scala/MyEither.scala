sealed trait MyEither[+E, +A] {
  /**
   * The given function is applied if this is a Right
   */
  def map[B](f: A => B): MyEither[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }

  /**
   * Binds the given function across Right
   */
  def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B] = this match {
    case Right(a) => f(a)
    case Left(e) => Left(e)
  }

  /**
   * Returns this Right or the given argument if this is a Left
   */
  def orElse[EE >: E, B >: A](b: => MyEither[EE, B]): MyEither[EE, B] = this match {
    case Right(a) => Right(a)
    case Left(_) => b
  }

  /**
   * Combines two MyEither values using a binary function
   */
  def map2[EE >: E, B, C](b: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] =
    for {
      a <- this
      bb <- b
    } yield f(a, bb)
}
case class Left[+E](value: E) extends MyEither[E, Nothing]
case class Right[+A](value: A) extends MyEither[Nothing, A]
object MyEither {
  /**
   * Combines a MyList of MyEither into one MyEither containing a MyList
   * of all the MyEither values in the original MyList
   */
  def sequence[E, A](es: MyList[MyEither[E, A]]): MyEither[E, MyList[A]] = traverse(es)(x => x)

  /**
   * Maps over a MyList using a function that might fail,
   * returning Right(Nil) if applying it to any element of the MyList returns an error.
   */
  def traverse[E, A, B](as: MyList[A])(f: A => MyEither[E, B]): MyEither[E, MyList[B]] = as match {
    case Nil => Right(Nil)
    case Cons(h, t) => f(h).map2(traverse(t)(f))(_ :: _)
  }
}
