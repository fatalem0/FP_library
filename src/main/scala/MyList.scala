import scala.annotation.tailrec

sealed trait MyList[+A] {
  /**
   * Prepends a value to a MyList
   */
  def ::[AA >: A](value: AA): MyList[AA] = Cons(value, this)

  /**
   * Removes the first element of a MyList
   */
  def tail(): MyList[A] = this match {
    case Nil => throw new UnsupportedOperationException("The given list is empty")
    case Cons(_, xs) => xs
  }

  /**
   * Replaces the first element of a MyList with a given value
   */
  def setHead[AA >: A](value: AA): MyList[AA] = this match {
    case Nil => throw new UnsupportedOperationException()
    case Cons(_, xs) => Cons(value, xs)
  }

  /**
   * Removes the first n elements from a MyList
   */
  def drop(n: Int): MyList[A] = {
    @tailrec
    def go(l: MyList[A], n: Int): MyList[A] = l match {
      case Nil => l
      case Cons(_, xs) if n == 0 => xs
      case Cons(_, xs)=> go(xs, n - 1)
    }

    if (n < 0) throw new IllegalArgumentException("n is less than zero!")
    else if (n == 0) this
    else go(this, n - 1)
  }

  /**
   * Removes elements from the MyList prefix as long as they match a predicate f
   */
  def dropWhile(f: A => Boolean): MyList[A] = {
    @tailrec
    def go(l: MyList[A]): MyList[A] = l match {
      case Cons(x, xs) if f(x) => go(xs)
      case _ => l
    }

    go(this)
  }

  /**
   * Reverses a MyList
   */
  def reverse(): MyList[A] = {
    @tailrec
    def go(l: MyList[A], resList: MyList[A] = Nil): MyList[A] = l match {
      case Nil => resList
      case Cons(head, xs) => go(xs, head :: resList)
    }

    go(this)
  }

  /**
   * Removes the last element from a MyList
   */
  def init(): MyList[A] = this.reverse().tail().reverse()


  /**
   * SECOND IMPL: Removes the last element from a MyList
   */
  def init2(): MyList[A] = {
    import scala.collection.mutable.ListBuffer

    val buffer = new ListBuffer[A]

    @tailrec
    def go(l: MyList[A]): MyList[A] = l match {
      case Nil => throw new UnsupportedOperationException("The given list is empty")
      case Cons(_, Nil) => MyList(buffer.toList: _*)
      case Cons(x, xs) =>
        buffer += x
        go(xs)
    }

    go(this)
  }

  /**
   * Computes the length of a MyList
   */
  def length(): Int = this.foldRight(0)((_, acc) => acc + 1)

  /**
   * Applies a function to all elements of this MyList and a start value, going right to left (not stack-safe)
   */
  def foldRight[B](z: B)(f: (A, B) => B): B = this match {
    case Nil => z
    case Cons(x, xs) => f(x, xs.foldRight(z)(f))
  }

  /**
   * Applies a function to all elements of this MyList and a start value, going left to right (stack-safe)
   */
  def foldLeft[B](z: B)(f: (B, A) => B): B = {
    @tailrec
    def go(l: MyList[A], z: B)(f: (B, A) => B): B = l match {
      case Nil => z
      case Cons(x, xs) => go(xs, f(z, x))(f)
    }

    go(this, z)(f)
  }

  /**
   * The foldLeft function implemented via the foldRight function
   */
  def foldLeftViaFoldRight[B](z: B)(f: (A, B) => B): B =
    this.reverse().foldRight(z)((a, b) => f(a, b))

  /**
   * The foldRight function implemented via the foldLeft function
   */
  def foldRightViaFoldLeft[B](z: B)(f: (A, B) => B): B =
    this.reverse().foldLeft(z)((a, b) => f(b, a))


  /**
   * Appends a MyList xs via the foldRight function
   */
  def appendViaFoldRight[AA >: A](xs: MyList[AA]): MyList[AA] =
    this.foldRight(xs)(Cons(_, _))

  /**
   * Makes a string from a MyList with a delimiter
   */
  def mkString(delimiter: String): String = {
    @tailrec
    def delimiting(list: MyList[A], str: String = ""): String = list match {
      case Nil => str
      case Cons(head, tail) if str.nonEmpty => delimiting(tail, s"$str$delimiter$head")
      case Cons(head, tail) => delimiting(tail, s"$head")
    }

    delimiting(this)
  }

  /**
   * Generalizes modifying each element in a MyList while maintaining the structure of the MyList
   */
  def map[B](f: A => B): MyList[B] = {
    @tailrec
    def go(l: MyList[A], resList: MyList[B] = Nil): MyList[B] = l match {
      case Nil => resList
      case Cons(x, xs) => go(xs, f(x) :: resList)
    }

    go(this).reverse()
  }

  /**
   * Removes elements from a MyList unless they satisfy a given predicate
   */
  def filter(f: A => Boolean): MyList[A] = {
    @tailrec
    def go(l: MyList[A], resList: MyList[A] = Nil): MyList[A] = l match {
      case Nil => resList
      case Cons(x, xs) => if (f(x)) go(xs, x :: resList) else go(xs, resList)
    }

    go(this).reverse()
  }

  /**
   * Works like the 'map', but will return a MyList[B] instead of a single result
   */
  def flatMap[B](f: A => MyList[B]): MyList[B] =
    map(f).reverse().foldRightViaFoldLeft(MyList(): MyList[B])((el, acc) => acc.appendViaFoldRight(el))

  /**
   * The 'filter' method implementation via flatMap
   */
  def filterViaFlatMap(f: A => Boolean): MyList[A] = flatMap(el => if (f(el)) MyList(el) else Nil)

  /**
   * Applies a function to a pair of elements from two MyList and creates a new MyList
   */
  def zipWith[B, C](b: MyList[B], f: (A, B) => C): MyList[C] = {
    @tailrec
    def go(xs: MyList[A], ys: MyList[B], resList: MyList[C]): MyList[C] = (xs, ys) match {
      case (Nil, _) => resList
      case (_, Nil) => resList
      case (Cons(x, xs), Cons(y, ys)) => go(xs, ys, f(x, y) :: resList)
    }

    go(this, b, Nil).reverse()
  }
}

case object Nil extends MyList[Nothing]
case class Cons[+A](head: A, xs: MyList[A]) extends MyList[A] {
  override def toString: String = s"MyList(${mkString(", ")})"
}

object MyList {
  /**
   * Computes the sum of all elements of a given MyList
   */
  def sum2(ints: MyList[Int]): Int =
    ints.foldRight(0)(_ + _)

  /**
   * Computes the product of all elements of a given MyList
   */
  def product2(ds: MyList[Double]): Double =
    ds.foldRight(1.0)(_ * _)

  /**
   * Transforms a MyList of integers by adding 1 to each element
   */
  def inc(l: MyList[Int]): MyList[Int] =
    l.foldRightViaFoldLeft(Nil: MyList[Int])((el, acc) => Cons(el + 1, acc))

  /**
   * Turns each value in a MyList[Double] into a String
   */
  def convertToString(l: MyList[Double]): MyList[String] =
    l.foldRightViaFoldLeft(Nil: MyList[String])((el, acc) => Cons(el.toString, acc))

  /**
   * Constructs a new MyList by adding corresponding elements
   */
  def sumLists(a: MyList[Int], b: MyList[Int]): MyList[Int] = {
    @tailrec
    def go(xs: MyList[Int], ys: MyList[Int], resList: MyList[Int]): MyList[Int] = (xs, ys) match {
      case (Nil, _) => resList
      case (_, Nil) => resList
      case (Cons(x, xs), Cons(y, ys)) => go(xs, ys, Cons(x + y, resList))
    }

    go(a, b, Nil).reverse()
  }

  /**
   * Checks whether a MyList contains another MyList as a subsequence
   */
  @tailrec
  final def hasSubsequence(sup: MyList[Int], sub: MyList[Int]): Boolean = sup match {
    case Nil => sub == Nil
    case Cons(_, xs) =>
      @tailrec
      def go(list: MyList[Int], prefix: MyList[Int]): Boolean = (list, prefix) match {
        case (_, Nil) => true
        case (Cons(a, as), Cons(b, bs)) if a == b => go(as, bs)
        case _ => false
      }

      if (go(sup, sub)) {
        true
      } else {
        hasSubsequence(xs, sub)
      }
  }

  /**
   * Constructs a MyList instance
   */
  def apply[A](as: A*): MyList[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
