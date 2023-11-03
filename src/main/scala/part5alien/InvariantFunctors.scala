package part5alien

import cats.Monoid

object InvariantFunctors {

  trait Crypto[A] { self =>
    def encrypt(value: A): String
    def decrypt(encrypted: String): A

    def imap[B](back: B => A, forth: A => B): Crypto[B] = new Crypto[B] {
      override def encrypt(value: B): String = self.encrypt(back(value))

      override def decrypt(encrypted: String): B = forth(self.decrypt(encrypted))
    }
  }

  def encrypt[A](value: A)(using crypto: Crypto[A]): String = crypto.encrypt(value)
  def decrypt[A](repr: String)(using crypto: Crypto[A]): A  = crypto.decrypt(repr)

  given casesarCypher: Crypto[String] with {
    override def encrypt(value: String): String     = value.map(c => (c + 2).toChar)
    override def decrypt(encrypted: String): String = encrypted.map(c => (c - 2).toChar)
  }

  /*
    How can we support ints, doubles, Option[String]?
   */
  given doubleCrypto: Crypto[Double] = casesarCypher.imap(_.toString, _.toDouble)

  // TODO 1 - support Option[String]
  given optionStringCrypto: Crypto[Option[String]] = casesarCypher.imap(_.getOrElse(""), Option(_))

  // TODO 2 - if you have a Crypto[T] => Crypto[Option[T]] if you have a Monoid[T] in scope
  given optionCrypto[T: Monoid](using crypto: Crypto[T]): Crypto[Option[T]] =
    crypto.imap(_.getOrElse(Monoid[T].empty), Option(_))

  import cats.Invariant
  import cats.Show
  import cats.instances.string.*
  val showString       = Show[String]
  val showOptionString = Invariant[Show].imap(showString)(Option(_))(_.getOrElse(""))

  import cats.syntax.invariant.*
  val showOptionString2 = showString.imap(Option(_))(_.getOrElse("")) // identical

  // TODO - establish the relationship among the 3 type classes
  // What's the relationship? Who has the stronger type class?
  trait MyInvariant[W[_]] {
    def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B]
  }

  // apply transformations in reverse order it was written
  trait MyContravariant[W[_]] extends MyInvariant[W] {
    def contramap[A, B](wa: W[A])(back: B => A): W[B]
    override def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B] = contramap(wa)(back)
  }

  // apply transformations in the same order it was written
  trait MyFunctor[W[_]] extends MyInvariant[W] { // "covariant" functor
    def map[A, B](wa: W[A])(forth: A => B): W[B]
    override def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B] = map(wa)(forth)
  }

  def main(args: Array[String]): Unit = {
    val encrypted = encrypt("Let's encrypt")
    val decrypted = decrypt[String](encrypted)

    println(encrypted)
    println(decrypted)

    println(encrypt(Math.PI))
    println(decrypt[Double](encrypt(Math.PI)))

    println(encrypt(Option("Let's encrypt")))
    println(decrypt[Option[String]](encrypted))

    import cats.instances.double.*
    println(encrypt(Option(Math.PI)))
    println(decrypt[Option[Double]](encrypt(Math.PI)))
  }
}
