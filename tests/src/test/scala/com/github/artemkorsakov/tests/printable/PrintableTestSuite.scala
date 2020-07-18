package com.github.artemkorsakov.tests.printable

import com.github.artemkorsakov.cats.Cat
import com.github.artemkorsakov.printable.PrintableInstances._
import com.github.artemkorsakov.printable.PrintableSyntax._
import com.github.artemkorsakov.printable._
import org.scalatest.Matchers
import org.scalatest.funsuite.AnyFunSuiteLike

class PrintableTestSuite extends AnyFunSuiteLike with Matchers {
  test("test Printable") {
    Printable.print("Test string")
    Printable.format("Test string") shouldBe "Test string"
    Printable.print(123)
    Printable.format(123) shouldBe "123"

    val cat: Cat = Cat("Garfield", 5, "white")
    Printable.print(cat)
    Printable.format(cat) shouldBe "Garfield is a 5 year-old white cat."
    cat.print
    cat.format shouldBe "Garfield is a 5 year-old white cat."
  }
}
