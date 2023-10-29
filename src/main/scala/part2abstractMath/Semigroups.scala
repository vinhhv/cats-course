package part2abstractMath

object Semigroups {

  // Semigroups COMBINE elements of the same type
  import cats.Semigroup
  import cats.instances.int.*

  val naturalIntSemigroup = Semigroup[Int]
  val intCombination      = naturalIntSemigroup.combine(2, 46) // addition

  import cats.instances.string.*
  val naturalStringSemigroup = Semigroup[String]
  val stringCombination =
    naturalStringSemigroup.combine("I love ", "Cats") // concatentation

  // specific API
  def reduceInts(list: List[Int]): Int =
    list.reduce(naturalIntSemigroup.combine)
  def reduceStrings(list: List[String]): String =
    list.reduce(naturalStringSemigroup.combine)

  // general API
  def reduceThings[T](list: List[T])(using semigroup: Semigroup[T]): T =
    list.reduce(semigroup.combine)

  // TODO 1: support a new type
  // hint: use the same pattern we used with Eq
  case class Expense(id: Long, amount: Double)

  given expenseSemigroup: Semigroup[Expense] = Semigroup.instance[Expense] {
    (x, y) => Expense(x.id.max(y.id), x.amount + y.amount)
  }

  // extension methods from Semigroup - |+|
  import cats.syntax.semigroup.*
  val anIntSum = 2 |+| 3 // requires the presence of an implicit Semigroup[Int]
  val aStringConcat    = "we like " |+| "semigroups"
  val aCombinedExpense = Expense(4, 80) |+| Expense(56, 46)

  // TODO 2: implement reduceThings2
  def reduceThings2[T: Semigroup](list: List[T]): T =
    list.reduce(_ |+| _)

  def main(args: Array[String]): Unit = {
    println(intCombination)
    println(stringCombination)

    // specific API
    val numbers = (1 to 10).toList
    println(reduceInts(numbers))

    val strings = List("I'm ", "starting ", "to ", "like ", "semigroups")
    println(reduceStrings(strings))

    // general API
    println(
      reduceThings(numbers)
    ) // compiler injects the implicit Semigroup[Int]
    println(
      reduceThings(strings)
    ) // compiler injects the implicit Semigroup[String]

    // compiler produces an implicit Semigroup[Option[Int]]
    import cats.instances.option.*

    val numberOptions: List[Option[Int]] = numbers.map(n => Option(n))
    println(
      reduceThings(numberOptions)
    ) // an Option[Int] containing the sum of all the numbers

    val stringOptions: List[Option[String]] = strings.map(s => Option(s))
    println(
      reduceThings(stringOptions)
    )

    // test ex 1
    val expenses = List(Expense(1, 99), Expense(2, 35), Expense(43, 10))
    println(reduceThings(expenses))

    // test ex 2
    println(reduceThings2(expenses))
  }
}
