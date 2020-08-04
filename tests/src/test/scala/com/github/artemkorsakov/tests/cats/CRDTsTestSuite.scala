package com.github.artemkorsakov.tests.cats

import com.github.artemkorsakov.cats.BoundedSemiLattice._
import com.github.artemkorsakov.cats.{ GCounter, GCounterOld }
import org.scalatest.Matchers
import org.scalatest.funsuite.AnyFunSuiteLike

class CRDTsTestSuite extends AnyFunSuiteLike with Matchers {
  test("11.2.3 Exercise: GCounter Implementation") {
    val a = GCounterOld[Int](Map("A" -> 2, "B" -> 3))
    a shouldBe GCounterOld[Int](Map("A"                   -> 2, "B" -> 3))
    a.increment("A", 4) shouldBe GCounterOld[Int](Map("A" -> 4, "B" -> 3))
    a.increment("B", 4) shouldBe GCounterOld[Int](Map("A" -> 2, "B" -> 4))

    val b = GCounterOld[Int](Map("A" -> 1, "B" -> 14))
    b shouldBe GCounterOld[Int](Map("A" -> 1, "B" -> 14))

    val c = a.increment("A", 4).increment("B", 4)
    c shouldBe GCounterOld(Map("A" -> 4, "B" -> 4))
    c.total shouldBe 4

    val d = c.merge(b)
    d shouldBe GCounterOld(Map("A" -> 4, "B" -> 14))
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

  test("11.4 Abstracting GCounter to a Type Class") {
    val g1 = Map("a" -> 7, "b" -> 3)
    val g2 = Map("a" -> 2, "b" -> 5)

    val counter = GCounter[Map, String, Int]

    val merged = counter.merge(g1, g2)
    merged shouldBe Map("a" -> 7, "b" -> 5)
    val total = counter.total(merged)
    total shouldBe 7
    val increment = counter.increment(g1)("a", 8)
    increment shouldBe Map("a" -> 8, "b" -> 3)
  }

}
