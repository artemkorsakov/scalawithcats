package com.github.artemkorsakov.tests.printable

import com.github.artemkorsakov.printable.Printable
import com.github.artemkorsakov.printable.PrintableInstances._
import org.scalatest.Matchers
import org.scalatest.funsuite.AnyFunSuiteLike

class PrintableTestSuite extends AnyFunSuiteLike with Matchers {
  test("test Printable") {
    Printable.format("Test string") shouldBe "Print string Test string"
    Printable.format(123) shouldBe "Print int 123"
    Printable.print("Test string")
    Printable.print(123)
  }
}
