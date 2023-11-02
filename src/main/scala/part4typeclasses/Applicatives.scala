package part4typeclasses

object Applicatives {

  // Applicatives = Functors + the pure method
  import cats.Applicative
  import cats.instances.list.*
  val listApplicative = Applicative[List]
  val aList           = listApplicative.pure(2) // List(2)

  import cats.instances.option.* // implicit Applicative[Option]
  val optionApplicative = Applicative[Option]
  val anOption          = optionApplicative.pure(2) // Some(2)

  // pure extension method
  import cats.syntax.applicative.*
  val aSweetList  = 2.pure[List]   // List(2)
  val SweetOption = 2.pure[Option] // Some(2)

  // Monads extend Applicatives
  // Applicatives extends Functors

  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  val aValidValue: ErrorsOr[Int]        = Validated.valid(43) // "pure"
  val aModifiedValidated: ErrorsOr[Int] = aValidValue.map(_ + 1)

  val validatedApplicative = Applicative[ErrorsOr]

  // TODO: thought experiment
  // def ap[W[_], B, T](wf: W[B => T])(wb: W[B]): W[T] = ??? // already implemented

  def productWithApplicatives[W[_], A, B](wa: W[A], wb: W[B])(implicit applicative: Applicative[W]): W[(A, B)] = {
    val functionWrapper: W[B => (A, B)] = applicative.map(wa)(a => (b: B) => (a, b) /* function */ )
    applicative.ap(functionWrapper)(wb)
  }

  // Applicatives have this ap[W[_], B, T](wf: W[B => T](wb: W[B]): W[T]
  // Applicatives can implement product from Semigroupal
  // => Applicatives extends Semigroupal

  def main(args: Array[String]): Unit = {}
}
