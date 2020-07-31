package com.github.artemkorsakov.tests.cats

import cats.data.NonEmptyList
import cats.instances.list._
import com.github.artemkorsakov.cats.MyKleisli._
import org.scalatest.Matchers
import org.scalatest.funsuite.AnyFunSuiteLike

class KleisliTestSuite extends AnyFunSuiteLike with Matchers {
  test("10.5 Kleislis. Preview") {
    val pipeline = step1 andThen step2 andThen step3

    pipeline.run(20) shouldBe List(42, 10, -42, -10, 38, 9, -38, -9)
  }

  test("10.5 Kleislis.") {
    createUser("Noel", "noel@underscore.io") shouldBe Right(User("Noel", "noel@underscore.io"))
    createUser("", "dave@underscore.io@io") shouldBe Left(NonEmptyList("Must be longer than 3 characters", List()))
  }

}
