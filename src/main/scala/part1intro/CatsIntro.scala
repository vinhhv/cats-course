package part1intro

object CatsIntro {
  // Eq
  // val aComparison = 2 == "a string"

  // part 1 - typeclass import
  import cats.Eq

  // part 2 - import TC instances for the types you need
  import cats.instances.int.*

  // part 3 - use the TC API
  val intEquality          = Eq[Int]
  val aTypeSafeComparision = intEquality.eqv(2, 3) // false
  // val anUnsafeComparison = intEquality.eqv(2, "a") // does not compile

  // part 4 - use extension methods
  import cats.syntax.eq.*
  val anotherTypeSafeComparison = 2 === 3
  val neqComparison             = 2 =!= 3 // true
  // val invalidComparison         = 2 === "a String" -- doesn't compile
  // extension methods are only visible in the presence of the right TC instance

  // part 5 - extending the TC operations to composite types, e.g. lists
  import cats.instances.list.* // we bring Eq[List[Int]] in scope
  val aListComparison = List(2) === List(3)

  // part 6 - create a TC instance for a custom type
  case class ToyCar(model: String, price: Double)

  given toyCarEq: Eq[ToyCar] = Eq.instance[ToyCar] { (car1, car2) =>
    car1.price == car2.price
  }

  val compareTwoToyCars =
    ToyCar("Ferrari", 29.99) === ToyCar("Lambo", 29.99) // true

  def main(args: Array[String]): Unit = {
    println(compareTwoToyCars)
  }
}
