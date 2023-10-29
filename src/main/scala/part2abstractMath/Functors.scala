package part2abstractMath

import scala.util.Try

object Functors {

  val aModifiedList   = List(1, 2, 3).map(_ + 1) // List(2, 3, 4)
  val aModifiedOption = Option(2).map(_ + 1)     // Some(3)
  val aModifiedTry    = Try(42).map(_ + 1)       // Success (43)

  // simplified definition
  trait MyFunctor[F[_]] {
    def map[A, B](initialValue: F[A])(f: A => B): F[B]
  }

  import cats.Functor
  import cats.instances.list.* // includes Functor[List]
  val listFunctor = Functor[List]

  val incrementedNumbers =
    listFunctor.map(List(1, 2, 3))(_ + 1) // List(2, 3, 4)

  import cats.instances.option.* // includes Functor[Option]
  val optionFunctor     = Functor[Option]
  val incrementedOption = optionFunctor.map(Option(2))(_ + 1) // Some(3)

  import cats.instances.try_.*
  val anIncrementedTry = Functor[Try].map(Try(42))(_ + 1) // Success(43)

  // generalizing an API
  def do10xList(list: List[Int]): List[Int]         = list.map(_ * 10)
  def do10xOption(option: Option[Int]): Option[Int] = option.map(_ * 10)
  def do10xTry(attempt: Try[Int]): Try[Int]         = attempt.map(_ * 10)

  // generalize
  def do10x[F[_]](container: F[Int])(using functor: Functor[F]): F[Int] =
    functor.map(container)(_ * 10)

  // TODO 1: define your own functor for a binary tree
  // hint: define an object which extends Functor[Tree]
  trait Tree[+T]
  object Tree {
    // "smart" constructors
    def leaf[T](value: T): Tree[T] = Leaf(value)
    def branch[T](value: T, left: Tree[T], right: Tree[T]): Tree[T] =
      Branch(value, left, right)
  }
  final case class Leaf[+T](value: T) extends Tree[T]
  final case class Branch[+T](value: T, left: Tree[T], right: Tree[T])
      extends Tree[T]

  given treeFunctor: Functor[Tree] with {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Branch(value, left, right) =>
        Branch(f(value), map(left)(f), map(right)(f))
      case Leaf(value) => Leaf(f(value))
    }
  }

  val tree         = Branch(1, Branch(2, Leaf(3), Leaf(4)), Leaf(5))
  val modifiedTree = treeFunctor.map(tree)(_ + 1)

  // extension method - map
  import cats.syntax.functor.*
  val tree_v1: Tree[Int] =
    Tree.branch(40, Tree.branch(5, Tree.leaf(10), Tree.leaf(30)), Tree.leaf(20))
  val incrementedTree = tree_v1.map(_ + 1)

  // TODO 2: write a shorter do10x method using extension methods
  def do10xShorter[F[_]: Functor](container: F[Int]): F[Int] =
    container.map(_ * 10)

  def main(args: Array[String]): Unit = {
    println(do10x(List(1, 2, 3)))
    println(do10x(Option(2)))
    println(do10x(Try(35)))
    println(modifiedTree)
    println(do10x(modifiedTree))

    // Needs type specification, since value is treated as Branch[T]
    println(do10x[Tree](Branch(1, Branch(2, Leaf(3), Leaf(4)), Leaf(5))))
    println(
      do10x(
        Tree.branch(1, Tree.branch(2, Tree.leaf(3), Tree.leaf(4)), Tree.leaf(5))
      )
    )

    println(incrementedTree)

    println(do10xShorter(incrementedTree))
  }
}
