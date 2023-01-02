sealed trait MyTree[+A] {
  /**
   * The number of nodes in a MyTree
   */
  private val size: Int = this match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size
  }

  /**
   * The maximum path length from the root of a MyTree to any leaf
   */
  private val depth: Int = this match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + l.depth.max(r.depth)
  }

  /**
   * Modifies each element in a MyTree with a given function
   */
  def map[B](f: A => B): MyTree[B] = this match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(l.map(f), r.map(f))
  }

  /**
   * Applies a function to all elements of this MyList and a start value, going right to left (not stack-safe)
   */
  def fold[B](f: A => B, g: (B, B) => B): B = this match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(l.fold(f, g), r.fold(f, g))
  }

  /**
   * The number of nodes in a MyTree via fold
   */
  val sizeViaFold: Int =
    fold(_ => 1, 1 + _ + _)

  /**
   * The maximum path length from the root of a MyTree to any leaf via fold
   */
  val depthViaFold: Int =
    fold(_ => 0, (d1, d2) => 1 + d1.max(d2))

  /**
   * Modifies each element in a MyTree with a given function via fold
   */
  def mapViaFold[B](f: A => B): MyTree[B] =
    fold(a => Leaf(f(a)), Branch(_, _))
}

case class Leaf[A](value: A) extends MyTree[A]
case class Branch[A](left: MyTree[A], right: MyTree[A]) extends MyTree[A]

object MyTree {

  /**
   * Returns the maximum element in a MyTree[Int]
   */
  def maximum(tree: MyTree[Int]): Int = tree match {
    case Leaf(a) => a
    case Branch(l, r) => maximum(l).max(maximum(r))
  }

  /**
   * Returns the maximum element in a MyTree[Int] via fold
   */
  def maximumViaFold(tree: MyTree[Int]): Int =
    tree.fold(a => a, _ max _)
}
