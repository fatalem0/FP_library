import scala.annotation.tailrec

object O2_chapter extends App {
  /**
   * The recursive function to get the nth Fibonacci number
   */
  def getFibonacciNumber(value: Int): Int = {
    @tailrec
    def go(value: Int, first: Int = 0, second: Int = 1): Int = value match {
      case 0 => second
      case _ => go(value - 1, second, first + second)
    }

    go(value)
  }

  /**
   * Checks whether an Array[A] is sorted according to a given comparison function
   */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def go(idx: Int): Boolean = {
      if (idx >= as.length - 1) true
      else if (ordered(as(idx), as(idx + 1))) false
      else go(idx + 1)
    }

    go(0)
  }

  /**
   * Converts a function f of two arguments into a function of one argument that partially applies f
   */
  def curry[A, B, C](f: (A, B) => C): A => B => C = {
    (a: A) => (b: B) => f(a, b)
  }

  /**
   * Reverses the transformation of the 'curry' function
   */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  /**
   * Composes two functions
   */
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }
}
