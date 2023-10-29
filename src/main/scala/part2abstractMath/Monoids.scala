package part2abstractMath

import cats.Semigroup
import cats.instances.int.*
import cats.syntax.semigroup.* // import the |+| extension method

object Monoids {

  val numbers = (1 to 1000).toList
  // |+| is always associative
  val sumLeft  = numbers.foldLeft(0)(_ |+| _)
  val sumRight = numbers.foldRight(0)(_ |+| _)

  // define a general API
  // def combineFold[T](list: List[T])(using semigroup: Semigroup[T]): T =
  //   list.foldLeft(/* WHAT?! */)(_ |+| _)

  // MONOIDS - same as Semigroup with a natural "neutral" value
  import cats.Monoid
  val intMonoid  = Monoid[Int]
  val combineInt = intMonoid.combine(23, 999) // 1022
  val zero       = intMonoid.empty            // 0

  import cats.instances.string.* // bring implicit Monoid[String] in scope
  val emptyString   = Monoid[String].empty // ""
  val combineString = Monoid[String].combine("I understand ", "monoids")

  import cats.instances.option.* // construct an implicit of Monoid[Option[Int]]
  val emptyOption = Monoid[Option[Int]].empty // None
  val combineOption = Monoid[Option[Int]]
    .combine(Option(2), Option.empty[Int]) // Option(2) => Some(2)
  val combineOption2 =
    Monoid[Option[Int]].combine(Option(3), Option(6)) // Some(9)

  // extension methods for Monoids - |+|
  import cats.syntax.monoid.* // either this one or cats.syntax.semigroup.*
  val combinedOptionFancy = Option(3) |+| Option(7)

  // TODO 1: implement a reduceByFold
  def combineFold[T](list: List[T])(using monoid: Monoid[T]): T =
    list.foldLeft(monoid.empty)(_ |+| _)

  // TODO 2: combine a list of phonebooks as Map[String, Int]
  import cats.instances.map.*
  val phonebooks = List(
    Map(
      "Alice" -> 235,
      "Bob"   -> 647
    ),
    Map(
      "Charlie" -> 372,
      "Daniel"  -> 889
    ),
    Map("Tina" -> 123)
  )
  val massivePhonebook = combineFold(phonebooks)

  // TODO 3 - shopping cart and online stores with Monoids
  // hint: define your own Monoid - Monoid.instance
  // hint: use combineByFold
  final case class ShoppingCart(items: List[String], total: Double)

  given shoppingCartMonoid: Monoid[ShoppingCart] =
    Monoid.instance[ShoppingCart](
      ShoppingCart(List.empty[String], 0.toDouble),
      { (sc1, sc2) =>
        ShoppingCart(sc1.items |+| sc2.items, sc1.total |+| sc2.total)
      }
    )

  def checkout(shoppingCarts: List[ShoppingCart]): ShoppingCart = combineFold(
    shoppingCarts
  )

  val sc1 = ShoppingCart(List("a", "b", "c"), 12.31)
  val sc2 = ShoppingCart(List("a1", "b1", "c1", "d"), 31.92)

  def main(args: Array[String]): Unit = {
    println(sumLeft)
    println(sumRight)
    println(combineFold(numbers))
    println(combineFold(List("I ", "like ", "monoids")))
    println(massivePhonebook)
    println(checkout(List(sc1, sc2)))
  }
}
