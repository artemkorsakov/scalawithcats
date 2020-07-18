package com.github.artemkorsakov.tests.cats

import java.util.Date

import cats.Eq
import cats.instances.int._
import cats.instances.option._
import cats.instances.string._
import cats.syntax.eq._
import cats.syntax.option._
import cats.syntax.show._
import com.github.artemkorsakov.cats.Cat
import com.github.artemkorsakov.cats.EqInstances._
import com.github.artemkorsakov.cats.ShowInstances._
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
}
