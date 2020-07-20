package com.github.artemkorsakov.tests.monsemi

import com.github.artemkorsakov.monsemi.Monoid
import com.github.artemkorsakov.monsemi.MonoidInstances._
import org.scalatest.Matchers
import org.scalatest.funsuite.AnyFunSuiteLike

class MonoidTestSuite extends AnyFunSuiteLike with Matchers {
  test("test boolean monoid") {
    Monoid.identityLaw(true) shouldBe true
    Monoid.identityLaw(false) shouldBe true

    Monoid.associativeLaw(true, true, true) shouldBe true
    Monoid.associativeLaw(true, true, false) shouldBe true
    Monoid.associativeLaw(true, false, true) shouldBe true
    Monoid.associativeLaw(true, false, false) shouldBe true
    Monoid.associativeLaw(false, true, true) shouldBe true
    Monoid.associativeLaw(false, true, false) shouldBe true
    Monoid.associativeLaw(false, false, true) shouldBe true
    Monoid.associativeLaw(false, false, false) shouldBe true

    Monoid.identityLaw(Set(1, 2)) shouldBe true
    Monoid.associativeLaw(Set(1, 2), Set(2, 3), Set(3, 4)) shouldBe true

    Monoid.identityLaw(Set("1", "2")) shouldBe true
    Monoid.associativeLaw(Set("1", "2"), Set("2", "3"), Set("3", "4")) shouldBe true
  }

}
