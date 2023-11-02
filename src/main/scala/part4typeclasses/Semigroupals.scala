package part4typeclasses

import cats.Monad

import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.{ExecutionContext, Future}

object Semigroupals {

  trait MySemigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  import cats.Semigroupal
  import cats.instances.option.* // implicit Semigroupal[Option]
  val optionSemigroupal = Semigroupal[Option]
  val aTupledOption     = optionSemigroupal.product(Some(123), Some("a string")) // Some((123, "a string"))
  val aNoneTupled       = optionSemigroupal.product(Some(123), None)             // None

  import cats.instances.future.* // implicit Semigroupal[Future]
  val es: ExecutorService    = Executors.newFixedThreadPool(8)
  given ec: ExecutionContext = ExecutionContext.fromExecutor(es)
  val aTupledFuture = Semigroupal[Future].product(Future("the meaning of life"), Future(42)) // Future("...", 42)

  import cats.instances.list.*
  import cats.syntax.flatMap.*
  import cats.syntax.functor.*
  val aTupledList = Semigroupal[List].product(List(1, 2), List("a", "b"))

  // TODO: implement product with monads
  def productWithMonads[F[_]: Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    for {
      a <- fa
      b <- fb
    } yield (a, b)

  // MONADS EXTEND SEMIGROUPALS

  // example: Validated
  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  val validatedSemigroupal = Semigroupal[ErrorsOr] // requires the implicit Semigroup[List[_]]

  val invalidsCombination = validatedSemigroupal.product(
    Validated.invalid(List("Something wrong", "Something else wrong")),
    Validated.invalid(List("This can't be right"))
  )
  // Validated is non-monadic semigroupal that doesn't short-circuit, losing "undesired" values

  type EitherErrorsOr[T] = Either[List[String], T]
  import cats.instances.either.* // implicit Monad[Either
  val eitherSemigroupal = Semigroupal[EitherErrorsOr]
  val eitherCombination = eitherSemigroupal.product( // in terms of map/flatMap
    Left(List("Something wrong", "Something else wrong")),
    Left(List("This can't be right"))
  )
  // loses errors because of the flatMap associativity law below

  // Associativity required by monadic law: m.flatMap(f).flatMap(g) == m.flatMap(x => f(x).flatMap(g))

  // TODO 2: define a Semigroupal[List] which does a zip
  val zipListSemigroupal: Semigroupal[List] = new Semigroupal[List] {
    override def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] = fa.zip(fb)
  }

  def main(args: Array[String]): Unit = {
    println(aTupledList)

    val aList1 = List(1, 2, 3)
    val aList2 = List('a', 'b')

    println(productWithMonads(aList1, aList2))

    println(invalidsCombination)
    println(eitherCombination)
    println(zipListSemigroupal.product(aList1, aList2))
    Thread.sleep(1000)
    es.shutdown()
  }
}
