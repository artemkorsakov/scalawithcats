package com.github.artemkorsakov.tests.cats

import cats.data.NonEmptyList
import cats.data.Validated.{ Invalid, Valid }
import com.github.artemkorsakov.cats.SectionTenPointFour._
import org.scalatest.Matchers
import org.scalatest.funsuite.AnyFunSuiteLike

class SectionTenPointFourTestSuite extends AnyFunSuiteLike with Matchers {
  test("10.4 Transforming Data") {
    createUser("Noel", "noel@underscore.io") shouldBe Valid(User("Noel", "noel@underscore.io"))

    createUser("", "dave@underscore.io@io") shouldBe Invalid(
      NonEmptyList(
        "Must be longer than 3 characters",
        List("Must contain a single @ character")
      )
    )
  }

}
