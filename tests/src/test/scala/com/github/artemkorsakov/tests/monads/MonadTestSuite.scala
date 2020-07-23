package com.github.artemkorsakov.tests.monads

import com.github.artemkorsakov.monads.MyMonadInstances._
import com.github.artemkorsakov.monads.{ MyId, MyMonad }
import org.scalatest.Matchers
import org.scalatest.funsuite.AnyFunSuiteLike

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class MonadTestSuite extends AnyFunSuiteLike with Matchers {
  test("test Option Monad") {
    def parseInt(str: String): Option[Int] =
      scala.util.Try(str.toInt).toOption

    def divide(a: Int, b: Int): Option[Int] =
      if (b == 0) None else Some(a / b)

    parseInt("abc") shouldBe None
    parseInt("123") shouldBe Some(123)
    divide(10, 0) shouldBe None
    divide(10, 2) shouldBe Some(5)

    def stringDivideBy(aStr: String, bStr: String): Option[Int] =
      for {
        aNum <- parseInt(aStr)
        bNum <- parseInt(bStr)
        ans  <- divide(aNum, bNum)
      } yield ans

    stringDivideBy("abc", "bcd") shouldBe None
    stringDivideBy("10", "bcd") shouldBe None
    stringDivideBy("abc", "10") shouldBe None
    stringDivideBy("abc", "0") shouldBe None
    stringDivideBy("10", "0") shouldBe None
    stringDivideBy("10", "2") shouldBe Some(5)

    (for {
      x <- (1 to 3).toList
      y <- (4 to 5).toList
    } yield (x, y)) shouldBe List((1, 4), (1, 5), (2, 4), (2, 5), (3, 4), (3, 5))
  }

  test("test Future Monad") {
    val doSomethingLongRunning: Future[Int]     = Future(100)
    val doSomethingElseLongRunning: Future[Int] = Future(200)

    val doSomethingVeryLongRunning: Future[Int] =
      for {
        result1 <- doSomethingLongRunning
        result2 <- doSomethingElseLongRunning
      } yield result1 + result2

    doSomethingVeryLongRunning.onComplete(sum => sum.get shouldBe 300)
  }

  test("test Monad laws") {
    val monad = MyMonad.apply[List]
    monad.pure(1) shouldBe List(1)
    val list = List("1", "2", "3")
    monad.flatMap(list)(x => List(x.toInt)) shouldBe List(1, 2, 3)
    monad.map(list)(_.toDouble) shouldBe List(1.0, 2.0, 3.0)
    monad.leftIdentity("1")(x => List(x.toInt)) shouldBe true
    val m = monad.pure("1")
    monad.rightIdentity(m) shouldBe true
    monad.associativity(m)(x => List(x.toInt))(x => List(x.toBinaryString)) shouldBe true
  }

  test("test MyId") {
    val a = MyId.pure(3)
    a shouldBe 3
    val b = MyId.flatMap(a)(_ + 1)
    b shouldBe 4
    val c = MyId.map(3)(_ + 5)
    c shouldBe 8
  }

  test("test Either") {
    val either1: Either[String, Int] = Right(10)
    val either2: Either[String, Int] = Right(32)

    (for {
      a <- either1
      b <- either2
    } yield a + b) shouldBe Right(42)

  }

}
