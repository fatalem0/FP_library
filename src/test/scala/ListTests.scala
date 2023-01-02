import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers.the

class ListTests extends AnyFlatSpec {
  "::" should "prepends a value to a MyList" in {
    assert(MyList(1, 2, 3).::(0).toString == "MyList(0, 1, 2, 3)")
    assert(MyList(1, 2, 3).::().toString == "MyList((), 1, 2, 3)")
  }

  "tail" should "remove the first element of a MyList" in {
    val ex = the [UnsupportedOperationException] thrownBy MyList().tail()

    assert(ex.getMessage == "The given list is empty")
    assert(MyList(1, 2, 3).tail().toString == "MyList(2, 3)")
  }

  "setHead" should "replace the first element of a MyList with a given value" in {
    assert(MyList(1, 2, 3).setHead().toString == "MyList((), 2, 3)")
    assert(MyList(1, 2, 3).setHead(0).toString == "MyList(0, 2, 3)")
    assert(MyList(1, 2, 3).setHead(1).toString == "MyList(1, 2, 3)")
  }

  "drop" should "remove the first n elements from a MyList" in {
    val ex = the [IllegalArgumentException] thrownBy MyList(1, 2, 3, 4, 5).drop(-1)

    assert(MyList(1, 2, 3, 4, 5).drop(0).toString == MyList(1, 2, 3, 4, 5).toString)
    assert(MyList(1, 2, 3, 4, 5).drop(3).toString == "MyList(4, 5)")
    assert(MyList(1, 2, 3, 4, 5).drop(4).toString == "MyList(5)")
    assert(MyList(1, 2, 3, 4, 5).drop(5).toString == "Nil")
    assert(MyList(1, 2, 3, 4, 5).drop(6).toString == "Nil")
    assert(ex.getMessage == "n is less than zero!")
  }

  "dropWhile" should "remove elements from the MyList prefix as long as they match a predicate f" in {
    assert(MyList(1, 2, 3, 4, 5).dropWhile(_ < 0).toString == "MyList(1, 2, 3, 4, 5)")
    assert(MyList(1, 2, 3, 4, 5).dropWhile(_ < 2).toString == "MyList(2, 3, 4, 5)")
    assert(MyList(1, 2, 3, 4, 5).dropWhile(_ < 3).toString == "MyList(3, 4, 5)")
    assert(MyList(1, 2, 3, 4, 5).dropWhile(_ < 6).toString == "Nil")
  }

  "reverse" should "reverse a MyList" in {
    assert(MyList(1, 2, 3).reverse().toString == "MyList(3, 2, 1)")
    assert(MyList().reverse().toString == "Nil")
  }

  "init" should "remove the last element from a MyList" in {
    val ex = the [UnsupportedOperationException] thrownBy MyList().init()

    assert(MyList(1, 2, 3, 4, 5).init().toString == "MyList(1, 2, 3, 4)")
    assert(MyList(1, 2).init().toString == "MyList(1)")
    assert(MyList(1).init().toString == "Nil")
    assert(ex.getMessage == "The given list is empty")
  }

  "init2" should "remove the last element from a MyList" in {
    val ex = the [UnsupportedOperationException] thrownBy MyList().init2()

    assert(MyList(1, 2, 3, 4, 5).init2().toString == "MyList(1, 2, 3, 4)")
    assert(MyList(1, 2).init2().toString == "MyList(1)")
    assert(MyList(1).init2().toString == "Nil")
    assert(ex.getMessage == "The given list is empty")
  }

  "length" should "compute the length of a MyList" in {
    assert(MyList(1, 2, 3, 4, 5).length().toString == "5")
    assert(MyList(1).length().toString == "1")
    assert(MyList().length().toString == "0")
  }

  "foldRight" should
    "apply a function to all elements of this MyList and a start value, going right to left (not stack-safe)" in
  {
    assert(MyList(1, 2, 3, 4, 5).foldRight(0)(_ + _).toString == "15")
    assert(MyList(1).foldRight(9)(_ + _).toString == "10")
  }

  "foldLeft" should
    "apply a function to all elements of this MyList and a start value, going left to right (stack-safe)" in
  {
    assert(MyList(1, 2, 3, 4, 5).foldLeft(0)(_ + _).toString == "15")
    assert(MyList(1).foldLeft(9)(_ + _).toString == "10")
  }

  "foldLeftViaFoldRight" should
    "imitate the behaviour of the foldLeft function implemented via the foldRight function" in
  {
    assert(MyList(1, 2, 3, 4, 5).foldLeftViaFoldRight(0)(_ + _).toString == "15")
    assert(MyList(1).foldLeftViaFoldRight(9)(_ + _).toString == "10")
  }

  "foldRightViaFoldLeft" should
    "imitate the behaviour of the foldRight function implemented via the foldLeft function" in
  {
    assert(MyList(1, 2, 3, 4, 5).foldRightViaFoldLeft(0)(_ + _).toString == "15")
    assert(MyList(1).foldRightViaFoldLeft(9)(_ + _).toString == "10")
  }

  "appendViaFoldRight" should "append a MyList xs via the foldRight function" in {
    assert(MyList(1, 2, 3).appendViaFoldRight(MyList(4, 5, 6)).toString == "MyList(1, 2, 3, 4, 5, 6)")
    assert(MyList(1, 2, 3).appendViaFoldRight(MyList(1, 2, 3)).toString == "MyList(1, 2, 3, 1, 2, 3)")
    assert(MyList(1, 2, 3).appendViaFoldRight(MyList()).toString == "MyList(1, 2, 3)")
    assert(MyList(1).appendViaFoldRight(MyList()).toString == "MyList(1)")

    assert(MyList().appendViaFoldRight(MyList(1, 2, 3)).toString == "MyList(1, 2, 3)")
    assert(MyList().appendViaFoldRight(MyList(1)).toString == "MyList(1)")
    assert(MyList().appendViaFoldRight(MyList()).toString == "Nil")
  }

  "mkString" should "make a string from a MyList with a delimiter" in {
    assert(MyList(1, 2, 3).mkString(" ") == "1 2 3")
    assert(MyList(1, 2, 3).mkString("*") == "1*2*3")
    assert(MyList(1).mkString(",") == "1")
    assert(MyList().mkString(" ") == "")
  }

  "sum2" should "compute the sum of all elements of a given MyList" in {
    assert(MyList.sum2(MyList(1, 2, 3)) == 6)
    assert(MyList.sum2(MyList(2)) == 2)
    assert(MyList.sum2(MyList(1)) == 1)
    assert(MyList.sum2(MyList()) == 0)
  }

  "product2" should "compute the product of all elements of a given MyList" in {
    assert(MyList.product2(MyList(2, 3, 4)) == 24)
    assert(MyList.product2(MyList(2)) == 2)
    assert(MyList.product2(MyList(1)) == 1)
    assert(MyList.product2(MyList()) == 1)
  }

  "inc" should "transform a MyList of integers by adding 1 to each element" in {
    assert(MyList.inc(MyList(1, 2, 3)).toString == "MyList(2, 3, 4)")
    assert(MyList.inc(MyList()).toString == "Nil")
  }

  "convertToString" should "turn each value in a MyList[Double] into a String" in {
    assert(MyList.convertToString(MyList(1.0, 2.0, 3.0)).toString == "MyList(1.0, 2.0, 3.0)")
    assert(MyList.convertToString(MyList()).toString == "Nil")
  }

  "map" should "generalize modifying each element in a MyList while maintaining the structure of the MyList" in {
    assert(MyList(1, 2, 3).map(_ + 1).toString == "MyList(2, 3, 4)")
    assert(MyList(1, 2, 3).map(_.toDouble).toString == "MyList(1.0, 2.0, 3.0)")
  }

  "filter" should "remove elements from a MyList unless they satisfy a given predicate" in {
    assert(MyList(1, 2, 3, 4, 5).filter(_ < 6).toString == "MyList(1, 2, 3, 4, 5)")
    assert(MyList(1, 2, 3, 4, 5).filter(_ < 2).toString == "MyList(1)")
    assert(MyList(1, 2, 3, 4, 5).filter(_ < 0).toString == "Nil")
  }

  "flatMap" should "work like the 'map', but will return a MyList[B] instead of a single result" in {
    assert(MyList(1, 2, 3).flatMap(i => MyList(i, i)).toString == "MyList(1, 1, 2, 2, 3, 3)")
  }

  "filterViaFlatMap" should "imitate the behaviour of the 'filter' method via flatMap" in {
    assert(MyList(1, 2, 3, 4, 5).filterViaFlatMap(_ < 6).toString == "MyList(1, 2, 3, 4, 5)")
    assert(MyList(1, 2, 3, 4, 5).filterViaFlatMap(_ < 2).toString == "MyList(1)")
    assert(MyList(1, 2, 3, 4, 5).filterViaFlatMap(_ < 0).toString == "Nil")
  }

  "sumLists" should "construct a new MyList by adding corresponding elements" in {
    assert(MyList.sumLists(MyList(1, 2, 3), MyList(4, 5, 6)).toString == "MyList(5, 7, 9)")
    assert(MyList.sumLists(Nil, MyList(4, 5, 6)).toString == "Nil")
    assert(MyList.sumLists(MyList(1, 2, 3), Nil).toString == "Nil")
  }

  "zipWith" should "apply a function to a pair of elements from two lists and create a new MyList" in {
    assert(MyList(1, 2, 3).zipWith(MyList(4, 5, 6), (x, y: Int) => x + y).toString == "MyList(5, 7, 9)")
    assert(MyList(1, 2, 3).zipWith(Nil, (x, y: Int) => x + y).toString == "Nil")
  }

  "hasSubsequence" should "check whether a MyList contains another MyList as a subsequence" in {
    assert(!MyList.hasSubsequence(MyList(1, 2, 3, 4), MyList(4, 5)))
    assert(!MyList.hasSubsequence(MyList(1, 2, 3, 4), MyList(4, 5)))
    assert(MyList.hasSubsequence(MyList(1, 2, 3, 4), MyList(1, 2)))
    assert(MyList.hasSubsequence(MyList(1, 2, 3, 4), MyList(3, 4)))
    assert(MyList.hasSubsequence(MyList(1, 2, 3), Nil))
    assert(!MyList.hasSubsequence(Nil, MyList(1, 2)))
    assert(MyList.hasSubsequence(Nil, Nil))
  }

  "apply" should "create a new MyList" in {
    assert(MyList(1, 2, 3).toString == "MyList(1, 2, 3)")
    assert(MyList().toString == "Nil")
  }
}
