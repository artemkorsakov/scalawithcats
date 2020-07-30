package com.github.artemkorsakov.tests.cats

import cats.instances.future._
import cats.instances.int._
import cats.instances.list._
import cats.instances.string._
import cats.syntax.traverse._
import cats.{ Monad, Monoid }
import com.github.artemkorsakov.cats.SectionNine.{ foldMap, parallelFoldMap }
import org.scalatest.Matchers
import org.scalatest.funsuite.AnyFunSuiteLike

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }

class SectionNineTestSuite extends AnyFunSuiteLike with Matchers {
  test("9 Case Study: Map-Reduce") {
    foldMap(Vector(1, 2, 3))(identity) shouldBe 6

    // Mapping to a String uses the concatenation monoid:
    foldMap(Vector(1, 2, 3))(_.toString + "! ") shouldBe "1! 2! 3! "

    // Mapping over a String to produce a String:
    foldMap("Hello world!".toVector)(_.toString.toUpperCase) shouldBe "HELLO WORLD!"
  }

  test("9.3.1 Futures, Thread Pools, and ExecutionContexts") {
    val future1 = Future {
      (1 to 100).toList.foldLeft(0)(_ + _)
    }
    Await.result(future1, 1.second) shouldBe 5050

    val future2 = Future {
      (100 to 200).toList.foldLeft(0)(_ + _)
    }
    Await.result(future2, 1.second) shouldBe 15150

    val future3 = future1.map(_.toString)
    Await.result(future3, 1.second) shouldBe "5050"

    val future4 = for {
      a <- future1
      b <- future2
    } yield a + b
    Await.result(future4, 1.second) shouldBe 20200

    val seq1 = Future.sequence(List(Future(1), Future(2), Future(3)))
    Await.result(seq1, 1.second) shouldBe List(1, 2, 3)

    val seq2 = List(Future(1), Future(2), Future(3)).sequence
    Await.result(seq2, 1.second) shouldBe List(1, 2, 3)

    Await.result(Monad[Future].pure(42), 1.second) shouldBe 42

    Await.result(Monoid[Future[Int]].combine(Future(1), Future(2)), 1.second) shouldBe 3
  }

  test("9.3.2 Dividing Work") {
    println(Runtime.getRuntime.availableProcessors)

    println((1 to 10).toList.grouped(3).toList)
  }

  test("9.3.3 Implementing parallelFoldMap") {
    Await.result(parallelFoldMap((0 to 1000).toVector)(identity), 1.second) shouldBe 1000 * 1001 / 2

    // Mapping to a String uses the concatenation monoid:
    Await.result(parallelFoldMap(Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))(_.toString + "! "), 1.second) shouldBe "1! 2! 3! 4! 5! 6! 7! 8! 9! 10! "

    // Mapping over a String to produce a String:
    Await.result(parallelFoldMap("Hello world!".toVector)(_.toString.toUpperCase), 1.second) shouldBe "HELLO WORLD!"

    Await.result(parallelFoldMap((1 to 1000000).toVector)(identity), 1.second) shouldBe 1784293664

    Await.result(parallelFoldMap((1 to 1000).toVector)(_ * 1000), 1.second) shouldBe 500500000
  }

}
