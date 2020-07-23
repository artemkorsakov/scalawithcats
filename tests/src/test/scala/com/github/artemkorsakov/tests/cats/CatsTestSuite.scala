package com.github.artemkorsakov.tests.cats

import java.util.Date

import cats._
import cats.instances.function._
import cats.instances.future._
import cats.instances.int._
import cats.instances.list._
import cats.instances.map._
import cats.instances.option._
import cats.instances.string._
import cats.instances.tuple._
import cats.instances.vector._
import cats.syntax.contravariant._
import cats.syntax.eq._
import cats.syntax.functor._
import cats.syntax.invariant._
import cats.syntax.option._
import cats.syntax.semigroup._
import cats.syntax.show._
import com.github.artemkorsakov.cats.Cat
import com.github.artemkorsakov.cats.EqInstances._
import com.github.artemkorsakov.cats.ShowInstances._
import com.github.artemkorsakov.monsemi.SuperAdderInstances._
import com.github.artemkorsakov.monsemi.{ Order, SuperAdder }
import org.scalatest.Matchers
import org.scalatest.funsuite.AnyFunSuiteLike

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._

class CatsTestSuite extends AnyFunSuiteLike with Matchers {
  test("test cats.Show") {
    123.show shouldBe "123"
    "abc".show shouldBe "abc"
    new Date().show should endWith("ms since the epoch.")
    Cat("Garfield", 5, "white").show shouldBe "Garfield is a 5 year-old white cat."
  }

  test("test cats.Eq") {
    val eqInt = Eq[Int]
    eqInt.eqv(123, 123) shouldBe true
    eqInt.eqv(123, 234) shouldBe false

    (123 eqv 123) shouldBe true
    (123 =!= 123) shouldBe false
    (123 neqv 123) shouldBe false

    (123 eqv 234) shouldBe false
    (123 =!= 234) shouldBe true
    (123 neqv 234) shouldBe true

    1.some eqv none[Int] shouldBe false
    1.some =!= none[Int] shouldBe true

    val x = new Date() // now
    x eqv x shouldBe true
    val y = new Date() // a bit later than now
    x eqv y shouldBe false

    val cat1       = Cat("Garfield", 38, "white")
    val cat2       = Cat("Heathcliff", 33, "orange and black")
    val cat3       = Cat("Heathcliff", 32, "orange and black")
    val cat4       = Cat("Heathcliff", 33, "black")
    val cat5       = Cat("Garfield", 33, "orange and black")
    val cat6       = Cat("Heathcliff", 33, "orange and black")
    val optionCat1 = cat1.some
    val optionCat2 = none[Cat]

    cat1 eqv cat1 shouldBe true
    cat1 eqv cat2 shouldBe false
    cat2 eqv cat3 shouldBe false
    cat2 eqv cat4 shouldBe false
    cat2 eqv cat5 shouldBe false
    cat2 eqv cat6 shouldBe true
    optionCat1 eqv optionCat2 shouldBe false
  }

  test("test cats.Monoid") {
    Monoid[String].combine("Hi ", "there") shouldBe "Hi there"
    Monoid[String].empty shouldBe ""
    Semigroup[String].combine("Hi ", "there") shouldBe "Hi there"
    Monoid[Int].combine(32, 10) shouldBe 42
    Monoid[Option[Int]].combine(Some(32), Some(10)) shouldBe Some(42)
    ("Hi " |+| "there" |+| Monoid[String].empty) shouldBe "Hi there"
    (1 |+| 2 |+| Monoid[Int].empty) shouldBe 3
  }

  test("test SuperAdder.add") {
    SuperAdder.add(List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) shouldBe 55
    SuperAdder.add(
      List(0.some, 1.some, None, 2.some, 3.some, 4.some, 5.some, None)
    ) shouldBe 15.some

    SuperAdder.add(
      List(Order(3.6, 17.5), Order(13.3, 22.5), Order(23.63, 33.0))
    ) shouldBe Order(40.53, 73.0)

    val map1 = Map("a" -> 1, "b" -> 2)
    val map2 = Map("b" -> 3, "d" -> 4)
    (map1 |+| map2) shouldBe Map("b" -> 5, "d" -> 4, "a" -> 1)

    val tuple1: (String, Int) = ("hello", 123)
    val tuple2: (String, Int) = ("world", 321)
    val tuple                 = tuple1 |+| tuple2
    tuple._1 shouldBe "helloworld"
    tuple._2 shouldBe 444

    SuperAdder.addAll(List(1, 2, 3)) shouldBe 6
    SuperAdder.addAll(List(None, Some(1), Some(2))) shouldBe Some(3)
  }

  test("test Functor Type") {
    val list1 = List(1, 2, 3)
    Functor[List].map(list1)(_ * 2) shouldBe List(2, 4, 6)
    Functor[List].map(list1)(_ / 2) shouldBe List(0, 1, 1)

    val option1 = Option(123)
    Functor[Option].map(option1)(_.toString) shouldBe Some("123")

    val func       = (x: Int) => x + 1
    val liftedFunc = Functor[Option].lift(func)
    liftedFunc(Option(1)) shouldBe Some(2)

    Functor[List].as(list1, "As") shouldBe List("As", "As", "As")

    val func1 = (a: Int) => a + 1
    val func2 = (a: Int) => a * 2
    val func3 = (a: Int) => s"$a!"
    val func4 = func1.map(func2).map(func3)

    func4(123) shouldBe "248!"

    def doMath[F[_]](start: F[Int])(implicit functor: Functor[F]): F[Int] =
      start.map(n => n + 1 * 2)
    doMath(Option(20)) shouldBe Some(22)
    doMath(List(1, 2, 3)) shouldBe List(3, 4, 5)
  }

  test("test Contravariant in Cats") {
    val showString = Show[String]

    val showSymbol = Contravariant[Show].contramap(showString)((sym: Symbol) => s"'${sym.name}'")

    showSymbol.show(Symbol("dave")) shouldBe "'dave'"

    showString
      .contramap[Symbol](sym => s"'${sym.name}'")
      .show(Symbol("dave")) shouldBe "'dave'"
  }

  test("test Invariant in Cats") {
    implicit val symbolMonoid: Monoid[Symbol] =
      Monoid[String].imap(Symbol.apply)(_.name)

    Monoid[Symbol].empty.toString() shouldBe "'"

    (Symbol("a") |+| Symbol("few") |+| Symbol("words"))
      .toString() shouldBe "'afewwords"
  }

  test("test Aside: Partial Unification") {
    val func1 = (x: Int) => x.toDouble
    val func2 = (y: Double) => y * 2

    val func3 = func1.map(func2)
    func3(5) shouldBe 10.0

    val either: Either[String, Int] = Right(123)
    either.map(_ + 1) shouldBe Right(124)

    val func3a: Int => Double =
      a => func2(func1(a))
    func3a(5) shouldBe 10.0

    val func3b: Int => Double =
      func2.compose(func1)
    func3b(5) shouldBe 10.0
  }

  test("test The Monad Type Class") {
    val opt1 = Monad[Option].pure(3)
    opt1 shouldBe Some(3)
    val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 2))
    opt2 shouldBe Some(5)
    val opt3 = Monad[Option].map(opt2)(a => 100 * a)
    opt3 shouldBe Some(500)

    val list1 = Monad[List].pure(3)
    list1 shouldBe List(3)
    val list2 = Monad[List].flatMap(List(1, 2, 3))(a => List(a, a * 10))
    list2 shouldBe List(1, 10, 2, 20, 3, 30)
    val list3 = Monad[List].map(list2)(a => a + 123)
    list3 shouldBe List(124, 133, 125, 143, 126, 153)

    Monad[Option].flatMap(Option(1))(a => Option(a * 2)) shouldBe Some(2)
    Monad[List].flatMap(List(1, 2, 3))(a => List(a, a * 10)) shouldBe List(1, 10, 2, 20, 3, 30)
    Monad[Vector].flatMap(Vector(1, 2, 3))(a => Vector(a, a * 10)) shouldBe Vector(1, 10, 2, 20, 3, 30)

    val fm     = Monad[Future]
    val future = fm.flatMap(fm.pure(1))(x => fm.pure(x + 2))
    Await.result(future, 1.second) shouldBe 3
  }

}
