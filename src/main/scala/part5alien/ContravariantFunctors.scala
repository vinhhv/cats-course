package part5alien

import cats.Monoid

object ContravariantFunctors {

  trait Format[T] { self => // contravariant type class
    def format(value: T): String

    def contramap[A](func: A => T): Format[A] = new Format[A] {
      override def format(value: A): String = self.format(func(value))
    }
  }

  def format[A](value: A)(using f: Format[A]) = f.format(value)

  given stringFormat: Format[String] with {
    override def format(value: String): String = "\"" + value + "\""
  }

  given intFormat: Format[Int] with {
    override def format(value: Int): String = value.toString
  }

  given booleanFormat: Format[Boolean] with {
    override def format(value: Boolean): String = if (value) "Y" else "N"
  }

  // problem: given Format[MyType], can we have a Format[Option[MyType]]?
  given getOptionFormat[T](using f: Format[T], m: Monoid[T]): Format[Option[T]] =
    f.contramap[Option[T]](_.getOrElse(m.empty))

  // given getOptionFormat[T] (using f: Format[T]): Format[Option[T]] with {
  //   override def format(value: Option[T]): String = f.format(value.get)
  // }

  /*
    IntFormat
    fo: Format[Option[Int]] = IntFormat.contramap[Option[Int]](_.get) // first get
    fo2: Format[Option[Option[Int]]] = fo.contramap[Option[Option[Int]]])(_.get) // second get

    fo2 = IntFormat
      .contramap[Option[Int]](_.get) // first get
      .contramap[Option[Option[Int]]](_.get) // second get

    fo2.format(Option(Option(42)) =
      fo1.format(secondGet(Option(Option(42))) =
      IntFormat.format(firstGet(secondGet(Option(Option(42))))

    order: = REVERSE from written order
    - second get
    - first get
    - format of Int

    Map applies transformations in sequence
    Contramap applies transformations in REVERSE
   */

  import cats.Contravariant
  import cats.Show
  import cats.instances.int.* // implicit Show[Int]
  val showInts                       = Show[Int]
  val showOptions: Show[Option[Int]] = Contravariant[Show].contramap(showInts)(_.getOrElse(0))

  import cats.syntax.contravariant.*
  val showOptionsShorter: Show[Option[Int]] = showInts.contramap(_.getOrElse(0))

  def main(args: Array[String]): Unit = {
    println(format("Nothing weird so far"))
    println(format(42))
    println(format(true))
    println(format(Option(42)))
    println(format(Option(Option(42))))
    println(format(Option(Option(Option(42)))))
  }
}
