package part2abstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Monads {

  // lists
  val numbersList = List(1, 2, 3)
  val charsList   = List('a', 'b', 'c')

  // TODO 1.1: how do you create all combinations of (number, char)?
  val combo: List[(Int, Char)] = for {
    num  <- numbersList
    char <- charsList
  } yield (num, char)

  // options
  val numberOption = Option(2)
  val charOption   = Option('d')

  // TODO 1.2: how do you create the combination of (number, char)?
  val comboOption = for {
    n <- numberOption
    c <- charOption
  } yield (n, c)

  // futures
  val executor = Executors.newFixedThreadPool(8)
  given ec: ExecutionContext =
    ExecutionContext.fromExecutorService(executor)

  val numberFuture = Future(42)
  val charFuture   = Future('Z')

  // TODO 1.3: how do you create the combination of (number, char)?
  val comboFuture = for {
    n <- numberFuture
    c <- charFuture
  } yield (n, c)

  /*
    Pattern
    - wrapping a value into a monadic value
    - the flatMap mechanism ; guarantees sequential

    MONADS (extends FUNCTORS)
    - higher-kinded type class that provides
      1. a pure method to wrap a normal value into a monadic value
      2. a flatMap method to transform monadic values in sequence

    Not iteration, but chained transformations
   */
  trait MyMonad[M[_]] {
    def pure[A](value: A): M[A]
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    // TODO implement this
    def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(f andThen pure)
  }

  import cats.Monad
  import cats.instances.option.* // implicit Monad[Option]
  val optionMonad = Monad[Option]
  val anOption    = optionMonad.pure(4) // Option(4) == Some(4)
  val aTransformedOption =
    optionMonad.flatMap(anOption)(x => if (x % 3 == 0) Some(x + 1) else None)

  import cats.instances.list.*
  val listMonad = Monad[List]
  val aList     = listMonad.pure(3)
  val aTransformedList =
    listMonad.flatMap(aList)(x => List(x, x + 1)) // List(4, 5)

  // TODO 2: use a Monad[Future]
  import cats.instances.future.*
  val futureMonad = Monad[Future] // requires an implicit ExecutionContext
  val aFuture     = futureMonad.pure(43)
  val aTransformedFuture = futureMonad.flatMap(aFuture)(x =>
    Thread.sleep(2000)
    Future(x + 44)
  ) // future that will end up with a Success(87)

  // specialized API
  def getPairsList(numbers: List[Int], chars: List[Char]): List[(Int, Char)] =
    numbers.flatMap(n => chars.map(c => (n, c)))

  def getPairsOption(
      number: Option[Int],
      char: Option[Char]
  ): Option[(Int, Char)] =
    number.flatMap(n => char.map(c => (n, c)))

  def getPairsFuture(
      number: Future[Int],
      char: Future[Char]
  ): Future[(Int, Char)] =
    number.flatMap(n => char.map(c => (n, c)))

  // generalize
  def getPairs[M[_], A, B](ma: M[A], mb: M[B])(implicit
      monad: Monad[M]
  ): M[(A, B)] =
    monad.flatMap(ma)(a => monad.map(mb)(b => (a, b)))

  // extension methods - weirder imports - pure, flatMap
  import cats.syntax.applicative.* // pure is here
  // implicit Monad[Option] will be use => Some(1)
  val oneOption = 1.pure[Option]
  val oneList   = 1.pure[List] // List(1)

  import cats.syntax.flatMap.* // flatMap is here
  val oneOptionTransformed = oneOption.flatMap(x => (x + 1).pure[Option])

  // TODO 3: implement the map method in MyMonad
  // Monads extends Functors
  val oneOptionMapped = Monad[Option].map(Option(2))(_ + 1)
  import cats.syntax.functor.* // map is here
  val oneOptionMapped2 = oneOption.map(_ + 2)

  // for-comprehensions
  val composedOptionFor = for {
    one <- 1.pure[Option]
    two <- 2.pure[Option]
  } yield one + two

  // TODO 4: implement a shorter version of getPairs using for-comprehensions
  def getPairsFor[M[_]: Monad, A, B](ma: M[A], mb: M[B]) =
    for {
      a <- ma
      b <- mb
    } yield (a, b)

  def main(args: Array[String]): Unit = {
    // println(combo)
    // println(comboOption)
    // println(comboFuture)
    // println(aTransformedFuture)

    println(getPairsFor(numbersList, charsList))
    println(getPairsFor(numberOption, charOption))
    getPairsFor(numberFuture, charFuture).foreach(println)
    executor.shutdown()
  }
}
