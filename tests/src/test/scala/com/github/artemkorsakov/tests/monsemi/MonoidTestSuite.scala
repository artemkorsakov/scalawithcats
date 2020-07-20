package com.github.artemkorsakov.tests.monsemi

import com.github.artemkorsakov.monsemi.MyMonoid
import com.github.artemkorsakov.monsemi.MyMonoidInstances._
import org.scalatest.Matchers
import org.scalatest.funsuite.AnyFunSuiteLike

class MonoidTestSuite extends AnyFunSuiteLike with Matchers {
  test("test boolean monoid") {
    MyMonoid.identityLaw(true) shouldBe true
    MyMonoid.identityLaw(false) shouldBe true

    MyMonoid.associativeLaw(true, true, true) shouldBe true
    MyMonoid.associativeLaw(true, true, false) shouldBe true
    MyMonoid.associativeLaw(true, false, true) shouldBe true
    MyMonoid.associativeLaw(true, false, false) shouldBe true
    MyMonoid.associativeLaw(false, true, true) shouldBe true
    MyMonoid.associativeLaw(false, true, false) shouldBe true
    MyMonoid.associativeLaw(false, false, true) shouldBe true
    MyMonoid.associativeLaw(false, false, false) shouldBe true

    MyMonoid.identityLaw(Set(1, 2)) shouldBe true
    MyMonoid.associativeLaw(Set(1, 2), Set(2, 3), Set(3, 4)) shouldBe true

    MyMonoid.identityLaw(Set("1", "2")) shouldBe true
    MyMonoid.associativeLaw(Set("1", "2"), Set("2", "3"), Set("3", "4")) shouldBe true
  }

}
