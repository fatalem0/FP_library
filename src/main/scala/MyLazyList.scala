import MyLazyList.empty

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
  def existsViaFoldRight(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  /**
   * Applies a function to all elements of this MyLazyList and a start value, going right to left (not stack-safe)
   */
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }
}

case object Empty extends MyLazyList[Nothing]
case class Cons[+A](h: () => A, t: () => MyLazyList[A]) extends MyLazyList[A]

object MyLazyList {
  /**
   * A smart constructor for creating nonempty stream
   */
  def cons[A](hd: => A, tl: => MyLazyList[A]): MyLazyList[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  /**
   * A smart constructor for creating an empty stream of a particular type
   */
  def empty[A]: MyLazyList[A] = Empty

  /**
   * Constructs a MyLazyList instance
   */
  def apply[A](as: A*): MyLazyList[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
