package com.github.artemkorsakov.tests.functors

import cats.instances.function._
import cats.syntax.functor._
import com.github.artemkorsakov.functors.Tree._
import com.github.artemkorsakov.functors.{ Branch, Leaf, Tree }
import org.scalatest.Matchers
import org.scalatest.funsuite.AnyFunSuiteLike

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }

class FunctorsTestSuite extends AnyFunSuiteLike with Matchers {
  test("test Future") {
    val future: Future[String] =
      Future(123).map(n => n + 1).map(n => n * 2).map(n => s"$n!")
    Await.result(future, 1.second)

    future.value.get.get shouldBe "248!"
  }

  test("test Functions") {
    val func1: Int => Double =
      (x: Int) => x.toDouble

    val func2: Double => Double =
      (y: Double) => y * 2

    (func1 map func2)(1) shouldBe 2.0
    (func1 andThen func2)(1) shouldBe 2.0
    func2(func1(1)) shouldBe 2.0

    val func =
      ((x: Int) => x.toDouble).map(x => x + 1).map(x => x * 2).map(x => s"$x!")

    func(123) shouldBe "248.0!"
  }

  test("test Tree") {
    Tree.leaf(100).map(_ * 2) shouldBe Leaf(200)
    Tree.branch(Tree.leaf(10), Tree.leaf(20)).map(_ * 2) shouldBe Branch(Leaf(20), Leaf(40))
  }
}
