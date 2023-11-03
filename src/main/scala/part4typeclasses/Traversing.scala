package part4typeclasses

import cats.{Applicative, Foldable, Functor, Monad}

import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.{ExecutionContext, Future}

object Traversing {

  val es: ExecutorService    = Executors.newFixedThreadPool(8)
  given ec: ExecutionContext = ExecutionContext.fromExecutorService(es)

  val servers: List[String] = List("server-ci.rockthejvm.com", "server-staging.rockthejvm.com", "prod.rockthejvm.com")
  def getBandwidth(hostname: String): Future[Int] = Future(hostname.length * 80)

  val allBandwidths: Future[List[Int]] = servers.foldLeft(Future(List.empty[Int])) { (accumulator, hostname) =>
    val bandFuture: Future[Int] = getBandwidth(hostname)
    for {
      accBandwidths <- accumulator
      band          <- bandFuture
    } yield accBandwidths :+ band
  }
  /*
    we have
    - a List[String]
    - a funcString => Future[Int]
    we want a Future[List[Int]]
   */

  val allBandwidthsTraverse: Future[List[Int]] = Future.traverse(servers)(getBandwidth)
  val allBandwidthsSequence: Future[List[Int]] = Future.sequence(servers.map(getBandwidth))

  import cats.syntax.applicative.*
  import cats.syntax.apply.*
  import cats.syntax.flatMap.*
  import cats.syntax.functor.*

  // TODO 1
  def listTraverseStronger[F[_]: Monad, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(Monad[F].pure(List.empty[B])) { (accumulator, a) =>
      for {
        b     <- func(a)
        accum <- accumulator
      } yield accum :+ b
    }

  def listTraverse[F[_]: Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (accumulator, a) =>
      val fb: F[B] = func(a)
      (accumulator, fb).mapN(_ :+ _)
    }

  // TODO 2
  def listSequence[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] =
    // listTraverse(list: List[F[A]])(func: F[A] => F[A]): F[List[A]]
    listTraverse(list)(identity)

  // TODO 3
  import cats.instances.vector.*
  val allPairs  = listSequence(List(Vector(1, 2), Vector(3, 4)))               // all the possible 2-tuples
  val allPairs2 = listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6))) // all the possible 3-tuples

  def filterAsOption(list: List[Int])(predicate: Int => Boolean): Option[List[Int]] =
    listTraverse[Option, Int, Int](list)(n => Some(n).filter(predicate))
  // TODO 4 - what's the result of:
  val filterPairs1 = filterAsOption(List(2, 4, 6))(_ % 2 == 0) // Some(List(2, 4, 6))
  val filterPairs2 = filterAsOption(List(1, 2, 3))(_ % 2 == 0) // None

  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  def filterAsValidated(list: List[Int])(predicate: Int => Boolean): ErrorsOr[List[Int]] =
    listTraverse[ErrorsOr, Int, Int](list) { n =>
      if (predicate(n)) Validated.valid(n)
      else Validated.invalid(List(s"predicate for $n failed"))
    }

  // TODO 5 - what's the result of:
  val allTrueValidated = filterAsValidated(List(2, 4, 6))(_ % 2 == 0) // Valid(List(2, 4, 6))
  val someFalseValidated = filterAsValidated(List(1, 2, 3))(
    _ % 2 == 0
  ) // Invalid(List("predicate for 1 is false", "predicate for 2 is false"))

  trait MyTraverse[L[_]] extends Foldable[L] with Functor[L] {
    def traverse[F[_]: Applicative, A, B](container: L[A])(func: A => F[B]): F[L[B]]
    def sequence[F[_]: Applicative, A](container: L[F[A]]): F[L[A]] =
      traverse(container)(identity)

    // TOOD 6
    // hint
    import cats.Id
    // type Identity[T] = T
    def map[A, B](wa: L[A])(f: A => B): L[B] = traverse[Id, A, B](wa)(f)
  }

  import cats.Traverse
  import cats.instances.future.* // Applicative[Future]
  val allbandwidthsCats = Traverse[List].traverse(servers)(getBandwidth)

  // extension methods
  import cats.syntax.traverse.* // sequence + traverse methods
  val allBandwidthsCats2 = servers.traverse(getBandwidth)

  def main(args: Array[String]): Unit = {
    println(allPairs)
    println(allPairs2)
    println(filterPairs1)
    println(filterPairs2)
    println(allTrueValidated)
    println(someFalseValidated)
    es.shutdown()
  }
}
