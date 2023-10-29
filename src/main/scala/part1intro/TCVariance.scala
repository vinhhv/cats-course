package part1intro

object TCVariance {
  import cats.Eq
  import cats.instances.int.*    // Eq[Int] TC instance
  import cats.instances.option.* // construct an Eq[Option[Int]] TC instance
  import cats.syntax.eq.*

  val aComparison = Option(2) === Option(3)
  // val anInvalidComparison = Some(2) === None // Eq[Some[Int]] not found

  // variance
  class Animal
  class Cat extends Animal

  // covariant type: subtyping is propagated to the generic type
  class Cage[+T]
  val cage: Cage[Animal] =
    new Cage[Cat] // Cat <: Animal, so Cage[Cat] <: Cage[Animal]

  // contravariant type: subtyping is propagated BACKWARDS to the generic type
  class Vet[-T]
  val vet: Vet[Cat] =
    new Vet[Animal] // Cat <: Animal, then Vet[Animal] v: Vet[Cat]

  // rule of thumb: "HAS a T" = covariant, "ACTS on T" = contravariant
  // variance affects how TC instances are being fetched

  // contravariant TC
  trait SoundMaker[-T]
  given animalSoundMaker: SoundMaker[Animal] with {}
  def makeSound[T](using soundMaker: SoundMaker[T]): Unit = println("wow")
  makeSound[Animal] // ok - TC instance defined above
  makeSound[Cat]    // ok - TC instance for Animal is also applicable to Cats

  // rule 1: contravariant TCs can use the superclass instances if nothing is available strictly for that type

  // has implications for subtypes
  given optionSoundMaker: SoundMaker[Option[Int]] with {}
  makeSound[Option[Int]]
  makeSound[Some[Int]]

  // covariant TC
  trait AnimalShow[+T] {
    def show: String
  }
  // given generalAnimalShow: AnimalShow[Animal] with {
  //   override def show: String = "animals everywhere"
  // }
  given catsShow: AnimalShow[Cat] with {
    override def show: String = "so many cats!"
  }
  def organizeShow[T](using event: AnimalShow[T]): String = event.show
  // rule 2: covariant TCs will always use the more specific TC instance for that type
  // but may confuse the compiler if the general TC is also present

  // rule 3: you can't have both benefits
  // Cats uses INVARIANT TCs
  Option(2) === Option.empty[Int]

  def main(args: Array[String]): Unit = {
    // ok - the compiler will inject CatsShow as implicit
    println(organizeShow[Cat])

    // will not compile in Scala 2 - ambiguous values - however it does in Scala 3, uses most specific TC instance
    println(organizeShow[Animal])
  }
}
