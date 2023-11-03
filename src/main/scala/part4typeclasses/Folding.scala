package part4typeclasses

import cats.{Eval, Monoid}
import cats.syntax.monoid.*
import part4typeclasses.Folding.ListExercises

object Folding {

  // TODO - implement all in terms of foldLeft
  object ListExercises {
    def map[A, B](list: List[A])(f: A => B): List[B] = list.foldRight(List.empty[B]) { (a, starting) =>
      f(a) :: starting
    }
    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] =
      list.foldLeft(List.empty[B]) { (currentList, a) =>
        currentList.foldRight(f(a))(_ :: _)
      }

    def filter[A](list: List[A])(predicate: A => Boolean): List[A] = list.foldRight(List.empty[A]) { (a, starting) =>
      if (predicate(a)) a :: starting
      else starting
    }
    def combineAll[A: Monoid](list: List[A]): A = list.foldRight(Monoid[A].empty)(_ |+| _)
  }

  import cats.Foldable
  import cats.instances.int.*  // implicit Foldable[Int]
  import cats.instances.list.* // implicit Foldable[List]
  val sum = Foldable[List].foldLeft(List(1, 2, 3), 0)(_ + _) // 6

  import cats.instances.option.* // implicit Foldable[Option]
  val sumOption = Foldable[Option].foldLeft(Option(2), 30)(_ + _) // 32

  // foldRight is stack-safe regardless of your container implementation
  val sumRight: Eval[Int] = Foldable[List].foldRight(List(1, 2, 3), Eval.now(0)) { (num, eval) =>
    eval.map(_ + num)
  }

  import cats.instances.int.* // implicit Foldable[Int]
  val anotherSum = Foldable[List].combineAll(List(1, 2, 3))
  import cats.instances.string.* // implicit Foldable[String]
  val mappedConcat = Foldable[List].foldMap(List(1, 2, 3))(_.toString) // implicit Monoid[String]

  // nesting
  import cats.instances.vector.*
  val intsNested = List(Vector(1, 2, 3), Vector(4, 5, 6))
  (Foldable[List] compose Foldable[Vector]).combineAll(intsNested)

  // extension methods
  import cats.syntax.foldable.*
  val sum3          = List(1, 2, 3).combineAll // req Foldable[List], Monoid[Int]
  val mappedConcat2 = List(1, 2, 3).foldMap(_.toString)

  def main(args: Array[String]): Unit = {
    import ListExercises.*

    val numbers = (1 to 10).toList

    println(map(numbers)(_ + 1))
    println(flatMap(numbers)(x => List(x, x + 1)))
    println(filter(numbers)(_ % 2 == 0))
    println(combineAll(numbers))
  }

}
