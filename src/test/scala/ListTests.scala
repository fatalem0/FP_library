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
    // TODO: add an example with Nil
  }

  "foldLeft" should
    "apply a function to all elements of this MyList and a start value, going left to right (stack-safe)" in
  {
    assert(MyList(1, 2, 3, 4, 5).foldLeft(0)(_ + _).toString == "15")
    assert(MyList(1).foldLeft(9)(_ + _).toString == "10")
    // TODO: add an example with Nil
  }

  "foldLeftViaFoldRight" should
    "imitate the behaviour of the foldLeft function implemented via the foldRight function" in
  {
    assert(MyList(1, 2, 3, 4, 5).foldLeftViaFoldRight(0)(_ + _).toString == "15")
    assert(MyList(1).foldLeftViaFoldRight(9)(_ + _).toString == "10")
    // TODO: add an example with Nil
  }

  "foldRightViaFoldLeft" should
    "imitate the behaviour of the foldRight function implemented via the foldLeft function" in
  {
    assert(MyList(1, 2, 3, 4, 5).foldRightViaFoldLeft(0)(_ + _).toString == "15")
    assert(MyList(1).foldRightViaFoldLeft(9)(_ + _).toString == "10")
    // TODO: add an example with Nil
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

  "apply" should "create a new MyList" in {
    assert(MyList(1, 2, 3).toString == "MyList(1, 2, 3)")
    assert(MyList().toString == "Nil")
  }
}
