package com.github.artemkorsakov.tests.cats

import cats.Semigroup
import cats.data.Validated
import cats.data.Validated.{ Invalid, Valid }
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.semigroup._
import com.github.artemkorsakov.cats.{ CheckF, CheckS }
import org.scalatest.Matchers
import org.scalatest.funsuite.AnyFunSuiteLike

class SectionTenTestSuite extends AnyFunSuiteLike with Matchers {
  test("10.3.0 Basic Combinators") {
    val semigroup = Semigroup[List[String]]

    // Combination using methods on Semigroup
    semigroup.combine(List("Badness"), List("More badness")) shouldBe List("Badness", "More badness")

    // Combination using Semigroup syntax
    List("Oh noes") |+| List("Fail happened") shouldBe List("Oh noes", "Fail happened")
  }

  test("10.3 Basic Combinators") {
    val a: CheckF[List[String], Int] =
      CheckF { v =>
        if (v > 2) v.asRight
        else List("Must be > 2").asLeft
      }

    val b: CheckF[List[String], Int] =
      CheckF { v =>
        if (v < -2) v.asRight
        else List("Must be < -2").asLeft
      }

    val check: CheckF[List[String], Int] =
      a and b

    check(5) shouldBe Left(List("Must be < -2"))
    check(0) shouldBe Left(List("Must be > 2", "Must be < -2"))
    check(-5) shouldBe Left(List("Must be > 2"))
  }

  test("10.3 Basic Combinators. Implementation 2") {
    val a: CheckS[List[String], Int] =
      CheckS.Pure { v =>
        if (v > 2) {
          Validated.valid(v)
        } else {
          Validated.invalid(List("Must be > 2"))
        }
      }

    val b: CheckS[List[String], Int] =
      CheckS.Pure { v =>
        if (v < -2) Validated.valid(v)
        else Validated.invalid(List("Must be < -2"))
      }

    val check: CheckS[List[String], Int] =
      a and b

    check(5) shouldBe Invalid(List("Must be < -2"))
    check(0) shouldBe Invalid(List("Must be > 2", "Must be < -2"))
    check(-5) shouldBe Invalid(List("Must be > 2"))

    val checkOr: CheckS[List[String], Int] =
      a or b

    checkOr(5) shouldBe Valid(5)
    checkOr(0) shouldBe Invalid(List("Must be > 2", "Must be < -2"))
    checkOr(-5) shouldBe Valid(-5)
  }

}
