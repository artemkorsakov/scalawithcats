package com.github.artemkorsakov.tests.cats

import java.util.Date

import cats.instances.int._
import cats.instances.string._
import cats.syntax.show._
import com.github.artemkorsakov.cats.Cat
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
}
