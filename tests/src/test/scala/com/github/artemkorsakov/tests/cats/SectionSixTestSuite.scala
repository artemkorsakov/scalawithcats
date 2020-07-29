package com.github.artemkorsakov.tests.cats

import cats.instances.either._
import cats.instances.future._
import cats.instances.int._
import cats.instances.invariant._
import cats.instances.list._
import cats.instances.option._
import cats.instances.string._
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.semigroup._
import cats.{ Monoid, Semigroupal, _ }
import com.github.artemkorsakov.cats.Transformative._
import org.scalatest.Matchers
import org.scalatest.funsuite.AnyFunSuiteLike

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._

class SectionSixTestSuite extends AnyFunSuiteLike with Matchers {
  test("6 Semigroupal and Applicative") {
    def parseInt(str: String): Either[String, Int] =
      Either.catchOnly[NumberFormatException](str.toInt).leftMap(_ => s"Couldn't read $str")

    for {
      a <- parseInt("a")
      b <- parseInt("b")
      c <- parseInt("c")
    } yield (a + b + c) shouldBe Left("Couldn't read a")
  }

  test("6.1.1 Joining Two Contexts") {
    Semigroupal[Option].product(Some(123), Some("abc")) shouldBe Some((123, "abc"))
    Semigroupal[Option].product(None, Some("abc")) shouldBe None
    Semigroupal[Option].product(Some(123), None) shouldBe None
  }

  test("6.1.2 Joining Three or More Contexts") {
    Semigroupal.tuple3(Option(1), Option(2), Option(3)) shouldBe Some((1, 2, 3))
    Semigroupal.tuple3(Option(1), Option(2), Option.empty[Int]) shouldBe None

    Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _) shouldBe Some(6)
    Semigroupal.map2(Option(1), Option.empty[Int])(_ + _) shouldBe None
  }

  test("6.2 Apply Syntax") {
    (Option(123), Option("abc")).tupled shouldBe Some((123, "abc"))
    (Option(123), Option("abc"), Option(true)).tupled shouldBe Some((123, "abc", true))

    final case class Cat(name: String, born: Int, color: String)

    (Option("Garfield"), Option(1978), Option("Orange & black")).mapN(Cat.apply) shouldBe Some(
      Cat("Garfield", 1978, "Orange & black")
    )
  }

  test("6.2.1 Fancy Functors and Apply Syntax") {
    final case class Cat(
        name: String,
        yearOfBirth: Int,
        favoriteFoods: List[String]
    )

    val tupleToCat: (String, Int, List[String]) => Cat =
      Cat.apply _

    val catToTuple: Cat => (String, Int, List[String]) =
      cat => (cat.name, cat.yearOfBirth, cat.favoriteFoods)

    implicit val catMonoid: Monoid[Cat] = (
      Monoid[String],
      Monoid[Int],
      Monoid[List[String]]
    ).imapN(tupleToCat)(catToTuple)

    val garfield   = Cat("Garfield", 1978, List("Lasagne"))
    val heathcliff = Cat("Heathcliff", 1988, List("Junk Food"))

    garfield |+| heathcliff shouldBe Cat("GarfieldHeathcliff", 3966, List("Lasagne", "Junk Food"))
  }

  test("6.3 Semigroupal Applied to Different Types") {
    val futurePair = Semigroupal[Future].product(Future("Hello"), Future(123))

    Await.result(futurePair, 1.second) shouldBe (("Hello", 123))

    case class Cat(name: String, yearOfBirth: Int, favoriteFoods: List[String])

    val futureCat = (
      Future("Garfield"),
      Future(1978),
      Future(List("Lasagne"))
    ).mapN(Cat.apply)

    Await.result(futureCat, 1.second) shouldBe Cat("Garfield", 1978, List("Lasagne"))

    Semigroupal[List].product(List(1, 2), List(3, 4)) shouldBe List((1, 3), (1, 4), (2, 3), (2, 4))

    Semigroupal[ErrorOrV].product(
      Left(Vector("Error 1")),
      Left(Vector("Error 2"))
    ) shouldBe Left(Vector("Error 1"))

    def product[F[_]: Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
      fa.flatMap(a => fb.map(b => (a, b)))

    product[List, Int, Int](List(1, 2), List(3, 4)) shouldBe List((1, 3), (1, 4), (2, 3), (2, 4))

    Semigroupal[List].product(List(1, 2), List(3, 4)) shouldBe List((1, 3), (1, 4), (2, 3), (2, 4))

    (List(1, 2), List(3, 4)).tupled shouldBe List((1, 3), (1, 4), (2, 3), (2, 4))
  }

}
