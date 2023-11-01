package part3datamanipulation

import cats.Semigroup

import scala.util.Try

object DataValidation {

  import cats.data.Validated

  val aValidValue: Validated[String, Int]    = Validated.valid(42) // "right" value
  val anInvalidValue: Validated[String, Int] = Validated.invalid("Something went wrong")

  val aTest: Validated[String, Int] = Validated.cond(42 > 39, 99, "meaning of life is too small")

  // TODO: use Either
  /*
    - n must be a prime
    - n must be non-negative
    - n <= 100
    - n must be even
   */
  def testNumber(n: Int): Either[List[String], Int] = {
    def isPrime: Option[String] =
      if (n < 2) return Some(s"$n is not prime")
      for (i <- 2 to math.sqrt(n).toInt) {
        if (n % i == 0) return Some(s"$n is not prime")
      }
      None

    def isNonNegative: Option[String] =
      if (n > 0) None
      else Some(s"$n is negative")

    def isLessThan100: Option[String] =
      if (n <= 100) None
      else Some(s"$n exceeds 100")

    def isEven: Option[String] =
      if (n % 2 == 0) None
      else Some(s"$n is not even")

    val results = List(isPrime, isNonNegative, isLessThan100, isEven)
    if (results.count(_.isEmpty) == 4) Right(n)
    else
      Left(
        results
          .map {
            case Some(value) => value
            case None        => ""
          }
          .filter(_.nonEmpty)
      )
  }

  import cats.instances.list.*
  given combineIntMax: Semigroup[Int] = Semigroup.instance[Int](Math.max)

  def validateNumber(n: Int): Validated[List[String], Int] =
    Validated
      .cond(n % 2 == 0, n, List("Number must be even"))
      .combine(Validated.cond(n >= 0, n, List("Number must be non-negative")))
      .combine(Validated.cond(n <= 100, n, List("Number must be <= 100")))

  // chain
  aValidValue.andThen(_ => anInvalidValue)
  // test a valid value
  aValidValue.ensure(List("something went wrong"))(_ % 2 == 0)
  // transform
  aValidValue.map(_ + 1)
  aValidValue.leftMap(_.length)
  aValidValue.bimap(_.length, _ + 1)
  // interoperate with stdlib
  val eitherToValidated: Validated[List[String], Int] = Validated.fromEither(Right(42))
  val optionToValidated: Validated[List[String], Int] = Validated.fromOption(None, List("Nothing present here"))
  val tryToValidated: Validated[Throwable, Int]       = Validated.fromTry(Try("something".toInt))
  // backwards
  aValidValue.toOption
  aValidValue.toEither

  // TODO 2 - form validation
  object FormValidation {
    import cats.instances.string.*

    type FormValidation[T] = Validated[List[String], T]

    def getValue(form: Map[String, String], fieldName: String): FormValidation[String] =
      Validated.fromOption(form.get(fieldName), List(s"The field $fieldName must be specified."))

    def nonBlank(value: String, fieldName: String): FormValidation[String] =
      Validated.cond(value.nonEmpty, value, List(s"The field $fieldName must not be blank."))

    def emailProperForm(email: String): FormValidation[String] =
      Validated.cond(email.contains("@"), email, List("Email is invalid."))

    def passwordCheck(password: String): FormValidation[String] =
      Validated.cond(password.length >= 10, password, List("Passwords must be at least 10 characters long."))

    /*
      fields are
      - name
      - email
      - password

      rules are
      - name, email and password MUST be specified
      - name must not be blank
      - email must have "@"
      - password must have >= 10 characters
     */
    def validateForm(form: Map[String, String]): FormValidation[String] = {
      getValue(form, "Name")
        .andThen(name => nonBlank(name, "Name"))
        .combine(getValue(form, "Email").andThen(emailProperForm))
        .combine(getValue(form, "Password").andThen(passwordCheck))
        .map(_ => "User registration complete.")
      // Validated
      //   .cond(form.contains("name"), "", List("Name must be provided"))
      //   .combine(Validated.cond(form.contains("email"), "", List("Email must be provided")))
      //   .combine(Validated.cond(form.contains("password"), "", List("Password must be provided")))
      //   .combine(Validated.cond(!form.get("name").contains(""), "", List("Name must not be blank")))
      //   .combine(Validated.cond(form.get("email").exists(_.contains("@")), "", List("Email does not contain '@'")))
      //   .combine(
      //     Validated
      //       .cond(
      //         form.get("password").exists(_.length >= 10),
      //         "Success",
      //         List("Password must be at least 10 characters")
      //       )
      //   )
    }
  }

  import cats.syntax.validated.*
  val aValidMeaningOfLife: Validated[List[String], Int] = 42.valid[List[String]]
  val anError: Validated[String, Int]                   = "Something went wrong".invalid[Int]

  def main(args: Array[String]): Unit = {
    // println(testNumber(2))
    // println(testNumber(3))
    // println(testNumber(7))
    // println(testNumber(10))
    // println(testNumber(101))
    // println(testNumber(-101))
    // println(testNumber(115))
    // println(validateNumber(115))
    val form = Map(
      "Name"     -> "Vinh",
      "Email"    -> "vinh@gmail.com",
      "Password" -> "password12345"
    )
    println(FormValidation.validateForm(form))
    val badForm = Map(
      "Name"     -> "",
      "Email"    -> "vinhgmail.com",
      "Password" -> "pa2345"
    )
    println(FormValidation.validateForm(badForm))
    val badForm2 = Map[String, String]()
    println(FormValidation.validateForm(badForm2))
  }
}
