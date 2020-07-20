package com.github.artemkorsakov.tests.cats

import java.util.Date

import cats.instances.int._
import cats.instances.map._
import cats.instances.option._
import cats.instances.string._
import cats.instances.tuple._
import cats.syntax.eq._
import cats.syntax.option._
import cats.syntax.semigroup._
import cats.syntax.show._
import cats.{Eq, Monoid, Semigroup}
import com.github.artemkorsakov.cats.Cat
import com.github.artemkorsakov.cats.EqInstances._
import com.github.artemkorsakov.cats.ShowInstances._
import com.github.artemkorsakov.monsemi.SuperAdderInstances._
import com.github.artemkorsakov.monsemi.{Order, SuperAdder}
import org.scalatest.Matchers
import org.scalatest.funsuite.AnyFunSuiteLike

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

    val cat1 = Cat("Garfield", 38, "white")
    val cat2 = Cat("Heathcliff", 33, "orange and black")
    val cat3 = Cat("Heathcliff", 32, "orange and black")
    val cat4 = Cat("Heathcliff", 33, "black")
    val cat5 = Cat("Garfield", 33, "orange and black")
    val cat6 = Cat("Heathcliff", 33, "orange and black")
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

    val tuple1: Tuple2[String, Int] = ("hello", 123)
    val tuple2: Tuple2[String, Int] = ("world", 321)
    val tuple = tuple1 |+| tuple2
    tuple._1 shouldBe "helloworld"
    tuple._2 shouldBe 444

    SuperAdder.addAll(List(1, 2, 3)) shouldBe 6
    SuperAdder.addAll(List(None, Some(1), Some(2))) shouldBe Some(3)
  }
}
