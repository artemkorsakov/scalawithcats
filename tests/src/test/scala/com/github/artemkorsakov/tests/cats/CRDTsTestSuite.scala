package com.github.artemkorsakov.tests.cats

import com.github.artemkorsakov.cats.GCounter
import org.scalatest.Matchers
import org.scalatest.funsuite.AnyFunSuiteLike

class CRDTsTestSuite extends AnyFunSuiteLike with Matchers {
  test("11.2.3 Exercise: GCounter Implementation") {
    val a = GCounter(Map("A" -> 2, "B" -> 3))
    a shouldBe GCounter(Map("A"                   -> 2, "B" -> 3))
    a.increment("A", 4) shouldBe GCounter(Map("A" -> 6, "B" -> 3))
    a.increment("B", 4) shouldBe GCounter(Map("A" -> 2, "B" -> 7))

    val b = GCounter(Map("A" -> 1, "B" -> 14))
    b shouldBe GCounter(Map("A" -> 1, "B" -> 14))

    val c = a.increment("A", 4).increment("B", 4)
    c shouldBe GCounter(Map("A" -> 6, "B" -> 7))
    c.total shouldBe 13

    val d = c.merge(b)
    d shouldBe GCounter(Map("A" -> 6, "B" -> 14))
    d.total shouldBe 20
  }

}
