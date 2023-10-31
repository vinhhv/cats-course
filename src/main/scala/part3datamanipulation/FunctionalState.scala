package part3datamanipulation

object FunctionalState {

  type MyState[S, A] = S => (S, A)

  import cats.data.State
  val countAndSay: State[Int, String] = State(currentCount => (currentCount + 1, s"Counted $currentCount"))

  val (eleven, counted10) = countAndSay.run(10).value

  // state = "iterative" computation

  var a = 10
  a += 1
  val firstComputation = s"Added 1 to 10, obtained $a"
  a *= 5
  val secondComputation = s"Multiplied with 5, obtained $a"

  // pure FP with states
  val firstTransformation  = State((s: Int) => (s + 1, s"Added 1 to 10, obtained ${s + 1}"))
  val secondTransformation = State((s: Int) => (s * 5, s"Multiplied with 5, obtained ${s * 5}"))
  val compositeTransformation: State[Int, (String, String)] = firstTransformation.flatMap { firstResult =>
    secondTransformation.map(secondResult => (firstResult, secondResult))
  }
  val compositeTransformation2 = for {
    firstResult  <- firstTransformation
    secondResult <- secondTransformation
  } yield (firstResult, secondResult)

  val func1 = (s: Int) => (s + 1, s"Added 1 to 10, obtained ${s + 1}")
  val func2 = (s: Int) => (s * 5, s"Multiplied with 5, obtained ${s * 5}")
  val compositeFunc = func1.andThen { case (newState, firstResult) =>
    (firstResult, func2(newState))
  }

  // TODO: an online store
  case class ShoppingCart(items: List[String], total: Double)
  def addToCart(item: String, price: Double): State[ShoppingCart, Double] = {
    State((sc: ShoppingCart) => (ShoppingCart(sc.items :+ item, sc.total + price), sc.total + price))
  }

  val danielsCart: State[ShoppingCart, Double] = for {
    _     <- addToCart("Fender guitar", 500)
    _     <- addToCart("Elixir strings", 19)
    total <- addToCart("Electric cable", 8)
  } yield total

  // TODO 2: pure mental gymnastics
  // returns a State data structure that, when run, will not change the state but will issue the value f(a)
  def inspect[A, B](f: A => B): State[A, B] = State(a => (a, f(a)))
  // returns a State data structure that, when run, returns the value of that state and make no changes
  def get[A]: State[A, A] = State(a => (a, a))
  // returns a State data structure that, when run, returns Unit and sets that state to that value
  def set[A](value: A): State[A, Unit] = State(_ => (value, ()))
  // returns a State data structure that, when run, will return Unit and sets that state to f(state)
  def modify[A](f: A => A): State[A, Unit] = State(a => (f(a), ()))

  // methods available
  import cats.data.State.*

  val program: State[Int, (Int, Int, Int)] = for {
    a <- get[Int]
    _ <- set[Int](a + 10)
    b <- get[Int]
    _ <- modify[Int](_ + 43)
    c <- inspect[Int, Int](_ * 2)
  } yield (a, b, c)

  def main(args: Array[String]): Unit = {
    println(compositeTransformation.run(10).value)
    println(compositeTransformation2.run(10).value)
    println(compositeFunc(10))

    val sc    = ShoppingCart(List("iPhone", "MacBook"), 1200.toDouble)
    val newSc = addToCart("AirPods", 100.toDouble).run(sc)
    println(newSc.value)

    println(danielsCart.run(ShoppingCart(List(), 0)).value)
  }
}
