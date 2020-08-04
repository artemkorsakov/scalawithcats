package com.github.artemkorsakov.tests.cats

import com.github.artemkorsakov.cats.BoundedSemiLattice._
import com.github.artemkorsakov.cats.GCounter
import org.scalatest.Matchers
import org.scalatest.funsuite.AnyFunSuiteLike

class CRDTsTestSuite extends AnyFunSuiteLike with Matchers {
  test("11.2.3 Exercise: GCounter Implementation") {
    val a = GCounter[Int](Map("A" -> 2, "B" -> 3))
    a shouldBe GCounter[Int](Map("A"                   -> 2, "B" -> 3))
    a.increment("A", 4) shouldBe GCounter[Int](Map("A" -> 4, "B" -> 3))
    a.increment("B", 4) shouldBe GCounter[Int](Map("A" -> 2, "B" -> 4))

    val b = GCounter[Int](Map("A" -> 1, "B" -> 14))
    b shouldBe GCounter[Int](Map("A" -> 1, "B" -> 14))

    val c = a.increment("A", 4).increment("B", 4)
    c shouldBe GCounter(Map("A" -> 4, "B" -> 4))
    c.total shouldBe 4

    val d = c.merge(b)
    d shouldBe GCounter(Map("A" -> 4, "B" -> 14))
    d.total shouldBe 14
  }

  test("11.3.2 Exercise: BoundedSemiLattice Instances") {
    intInstance.combine(1, 2) shouldBe 2
    intInstance.combine(2, 1) shouldBe 2
    intInstance.empty shouldBe 0
    intInstance.combine(1, intInstance.empty) shouldBe 1

    setInstance.combine(Set(1), Set(2)) shouldBe Set(1, 2)
    setInstance.combine(Set(2), Set(1)) shouldBe Set(1, 2)
    setInstance[Int].empty shouldBe Set.empty[Int]
    setInstance.combine(Set(1, 2, 3), setInstance[Int].empty) shouldBe Set(1, 2, 3)
  }

}
