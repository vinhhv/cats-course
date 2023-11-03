package part4typeclasses

import cats.{Applicative, Monad}

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

  def main(args: Array[String]): Unit = {
    println(allPairs)
    println(allPairs2)
    es.shutdown()
  }
}
