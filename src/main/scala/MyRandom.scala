trait MyRandom {
  /**
   * Returns a pseudo-random Int value
   */
  def nextInt: (Int, MyRandom)

  /**
   * Returns a non-negative counterpart after an invocation of nextInt
   */
  def nonNegativeInt: (Int, MyRandom)

  /**
   * Generates a Double between 0 and 1, not including 1
   */
  def double: (Double, MyRandom)

  /**
   * Generates an (Int, Double) pair
   */
  def intDouble: ((Int, Double), MyRandom)

  /**
   * Generates a (Double, Int) pair
   */
  def doubleInt: ((Double, Int), MyRandom)

  /**
   * Generates a (Double, Double, Double) 3-tuple
   */
  def double3: ((Double, Double, Double), MyRandom)
}

case class MySimpleRandom(seed: Long) extends MyRandom {
  def nextInt: (Int, MyRandom) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = MySimpleRandom(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  def nonNegativeInt: (Int, MyRandom) = {
    val (el, rnd) = nextInt

    val resEl =
      if (el == Int.MinValue) throw new NoSuchElementException("There's no non-negative counterpart")
      else if (el < 0) -el
      else el

    (resEl, rnd)
  }

  def double: (Double, MyRandom) = ???

  def intDouble: ((Int, Double), MyRandom) = ???

  def doubleInt: ((Double, Int), MyRandom) = ???

  def double3: ((Double, Double, Double), MyRandom) = ???
}