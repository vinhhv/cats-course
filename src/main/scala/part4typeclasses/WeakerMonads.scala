package part4typeclasses

import cats.{Applicative, Apply}

object WeakerMonads {

  trait MyFlatMap[M[_]] extends Apply[M] {
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    // TODO
    // hint: Apply extends Functor (access to map method)
    def ap[A, B](wf: M[A => B])(wa: M[A]): M[B] =
      flatMap(wa)(a => map(wf)(f => f(a)))
  }

  trait MyMonad[M[_]] extends Applicative[M] with MyFlatMap[M] {
    override def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(f andThen pure)
  }

  import cats.FlatMap
  import cats.syntax.flatMap.* // flatMap extension method
  import cats.syntax.functor.* // map extension method

  def getPairs[M[_]: FlatMap, A, B](as: M[A], bs: M[B]): M[(A, B)] = for {
    a <- as
    b <- bs
  } yield (a, b)

  // MONAD does not have its own new methods
  // MONAD = FlatMap + Applicative
  def main(args: Array[String]): Unit = {}
}
