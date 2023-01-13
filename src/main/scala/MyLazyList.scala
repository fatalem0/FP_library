import MyLazyList.{cons, empty}

import scala.annotation.tailrec

sealed trait MyLazyList[+A] {
  /**
   * A function to optionally extract the head of a MyLazyList
   */
  def headOption: MyOption[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  /**
   * Converts a MyLazyList to the regular MyList type
   */
  def toList: MyList[A] = {
    @tailrec
    def go(stream: MyLazyList[A], acc: MyList[A] = Nil): MyList[A] = stream match {
      case Empty => acc.reverse()
      case Cons(h, t) => go(t(), h() :: acc)
    }

    go(this)
  }

  /**
   * Reverses a MyLazyList
   */
  def reverse(): MyLazyList[A] = {
    @tailrec
    def go(l: MyLazyList[A], resList: MyLazyList[A] = Empty): MyLazyList[A] = l match {
      case Empty => resList
      case Cons(h, t) => go(t(), MyLazyList.cons(h(), resList))
    }

    go(this)
  }

  /**
   * Takes the first n elements from a MyLazyList
   */
  def take(n: Int): MyLazyList[A] = {
    @tailrec
    def go(lazyList: MyLazyList[A], n: Int, newLazyList: MyLazyList[A] = Empty): MyLazyList[A] = lazyList match {
      case Empty => newLazyList.reverse()
      case Cons(h, _) if n == 0 => go(empty, -1, MyLazyList.cons(h(), newLazyList))
      case Cons(h, t) => go(t(), n - 1, MyLazyList.cons(h(), newLazyList))
    }

    if (n < 0) throw new IllegalArgumentException("n is less than zero!")
    else if (n == 0) this
    else go(this, n - 1)
  }

  /**
   * Removes the first n elements from a MyLazyList
   */
  def drop(n: Int): MyLazyList[A] = {
    @tailrec
    def go(lazyList: MyLazyList[A], n: Int): MyLazyList[A] = lazyList match {
      case Empty => lazyList
      case Cons(_, t) if n == 0 => t()
      case Cons(_, t) => go(t(), n - 1)
    }

    if (n < 0) throw new IllegalArgumentException("n is less than zero!")
    else if (n == 0) this
    else go(this, n - 1)
  }

  /**
   * Takes elements from the MyLazyList prefix as long as they match a predicate p
   */
  def takeWhile(p: A => Boolean): MyLazyList[A] = {
    @tailrec
    def go(lazyList: MyLazyList[A], newLazyList: MyLazyList[A] = Empty): MyLazyList[A] = lazyList match {
      case Empty => newLazyList.reverse()
      case Cons(h, t) if p(h()) => go(t(), MyLazyList.cons(h(), newLazyList))
    }

    go(this)
  }

  /**
   * Checks whether an element matching a Boolean function exists in this MyLazyList
   */
  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  /**
   * Checks whether an element matching a Boolean function exists in this MyLazyList via foldRight
   */
  def existsViaFoldRight(p: A => Boolean): Boolean = foldRight(false)((el, acc) => p(el) || acc)

  /**
   * Applies a function to all elements of this MyLazyList and a start value, going right to left (not stack-safe)
   */
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  /**
   * Checks if all elements in the MyLazyList match a given predicate
   */
  def forAll(p: A => Boolean): Boolean = foldRight(true)((el, acc) => p(el) && acc)

  /**
   * Takes elements from the MyLazyList prefix as long as they match a predicate p implemented via foldRight
   */
  def takeWhileViaFoldRight(p: A => Boolean): MyLazyList[A] =
    foldRight(empty)((el, acc) => if (p(el)) cons(el, acc) else empty)

  /**
   * A function to optionally extract the head of a MyLazyList implemented via foldRight
   */
  def headOptionViaFoldRight: MyOption[A] = foldRight(None: MyOption[A])((el, _) => Some(el))

  /**
   * Generalizes modifying each element in a MyLazyList while maintaining the structure of the MyLazyList
   * implemented via foldRight
   */
  def mapViaFoldRight[B](f: A => B): MyLazyList[B] =
    foldRight(empty)((el, acc) => cons(f(el), acc))

  /**
   * Removes elements from a MyLazyList unless they satisfy a given predicate
   * implemented via foldRight
   */
  def filterViaFoldRight(f: A => Boolean): MyLazyList[A] =
    foldRight(empty)((el, acc) => if (f(el)) cons(el, acc) else acc)

  /**
   * Appends a MyLazyList xs via the foldRight function
   */
  def appendViaFoldRight[AA >: A](xs: MyLazyList[AA]): MyLazyList[AA] =
    foldRight(xs)(cons(_, _))

  /**
   * Works like the 'map', but will return a MyLazyList[B] instead of a single result
   */
  def flatMap[B](f: A => MyLazyList[B]): MyLazyList[B] =
    foldRight(empty[B])(f(_).appendViaFoldRight(_))

  /**
   * Returns an infinite loop of a given value
   */
  def constant[AA >: A](a: AA): MyLazyList[AA] = cons(a, constant(a))

  /**
   * Takes an initial value and a function for producing both the next state and the next value in
   * the generated MyLazyList
   */
  def unfold[S, B](z: S)(f: S => MyOption[(B, S)]): MyLazyList[B] = {
    f(z) match {
      case Some((h, t)) => cons(h, unfold(t)(f))
      case None => empty
    }
  }

  /**
   * Generates the infinite MyLazyList's of Fibonacci numbers via unfold
   */
  def fibsViaUnfold: MyLazyList[Int] =
    unfold((0, 1)) {
      case (current, next) => Some((next, (next, next + current)))
    }

  /**
   * Generates an infinite MyLazyList of integers, starting from n, then n + 1 and so on via unfold
   */
  def fromViaUnfold(n: Int): MyLazyList[Int] = unfold(n)(n => Some((n, n + 1)))

  /**
   * Returns an infinite loop of a given value via unfold
   */
  def constantViaUnfold[AA >: A](a: AA): MyLazyList[AA] = unfold(())(_ => Some((a, ())))

  /**
   * Creates an infitine MyLazyList of ones via unfold
   */
  def ones: MyLazyList[Int] = unfold(())(_ => Some((1, ())))

  /**
   * Generalizes modifying each element in a MyLazyList while maintaining the structure of the MyLazyList
   * implemented via unfold
   */
  def mapViaUnfold[B](f: A => B): MyLazyList[B] = unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }

  /**
   * Takes the first n elements from a MyLazyList
   */
  def takeViaUnfold(n: Int): MyLazyList[A] = unfold((this, n)) {
    case (Cons(h, _), 1) => Some((h(), (empty, 0)))
    case (Cons(h, t), n) if n > 1 => Some(h(), (t(), n - 1))
    case _ => None
  }

  /**
   * Takes elements from the MyLazyList prefix as long as they match a predicate p
   */
  def takeWhileViaUnfold(p: A => Boolean): MyLazyList[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _ => None
  }

  /**
   * Applies a function to a pair of elements from two MyLazyList and creates a new MyLazyList via unfold
   */
  def zipWith[B, C](b: MyLazyList[B], f: (A, B) => C): MyLazyList[C] = unfold((this, b)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
    case _ => None
  }

  /**
   * Should continue the traversal as long as either MyLazyList has more elements - it uses MyOption to
   * indicate whether each MyLazyList has been exhausted
   */
  def zipAll[B](s2: MyLazyList[B]): MyLazyList[(MyOption[A], MyOption[B])] = unfold((this, s2)) {
    case (Cons(h, t), Empty) => Some((Some(h()) -> None) -> (t() -> Empty))
    case (Empty, Cons(h, t)) => Some((None -> Some(h())) -> (Empty -> t()))
    case (Cons(h1, t1), Cons(h2, t2)) => Some(Some(h1()) -> Some(h2()) -> (t1() -> t2()))
    case _ => None
  }

  /**
   * Checks if one MyLazyList is a prefix of another
   */
  def startsWith[AA >: A](s: MyLazyList[AA]): Boolean = zipAll(s).takeWhile(_._2 != None).forAll {
    case (a, b) => a == b
  }

  /**
   * Returns the MyLazyList of sufixes of the input sequence, starting with the original MyLazyList
   * @return
   */
  def tails: MyLazyList[MyLazyList[A]] = unfold(this) {
    case Empty => None
    case Cons(h, t) => Some((Cons(h, t), t()))
  }.appendViaFoldRight(MyLazyList(empty))

  /**
   * Checks whether a MyLazyList contains a given subsequence
   */
  def hasSubsequence[AA >: A](s: MyLazyList[AA]): Boolean =
    tails exists (_ startsWith s)

  /**
   * Generalizes tails, and returns a MyLazyList of the intermediate results
   */
  def scanRight[B](init: B)(f: (A, => B) => B): MyLazyList[B] =
    foldRight(init -> MyLazyList(init)) { (a, b0) =>
      lazy val b1 = b0
      val b2 = f(a, b1._1)
      (b2, cons(b2, b1._2))
    }._2
}

case object Empty extends MyLazyList[Nothing]
case class Cons[+A](h: () => A, t: () => MyLazyList[A]) extends MyLazyList[A]

object MyLazyList {
  /**
   * A smart constructor for creating nonempty MyLazyList
   */
  def cons[A](hd: => A, tl: => MyLazyList[A]): MyLazyList[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  /**
   * A smart constructor for creating an empty MyLazyList of a particular type
   */
  def empty[A]: MyLazyList[A] = Empty

  /**
   * Generates an infinite MyLazyList of integers, starting from n, then n + 1 and so on
   */
  def from(n: Int): MyLazyList[Int] = cons(n, from(n + 1))

  /**
   * Generates the infinite MyLazyList's of Fibonacci numbers
   */
  def fibs: MyLazyList[Int] = {
    def go(current: Int = 0, next: Int = 1): MyLazyList[Int] =
      cons(current, go(next, current + next))

    go()
  }

  /**
   * Creates an infitine MyLazyList of ones
   */
  def ones: MyLazyList[Int] = MyLazyList.cons(1, ones)

  /**
   * Constructs a MyLazyList instance
   */
  def apply[A](as: A*): MyLazyList[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
