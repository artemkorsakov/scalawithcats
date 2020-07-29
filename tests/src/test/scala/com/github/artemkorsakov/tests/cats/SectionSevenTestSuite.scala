package com.github.artemkorsakov.tests.cats

import cats.data.Validated
import cats.data.Validated.{ Invalid, Valid }
import cats.instances.future._
import cats.instances.int._
import cats.instances.list._
import cats.instances.option._
import cats.instances.string._
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.foldable._
import cats.syntax.traverse._
import cats.{ Applicative, Foldable, Monoid, Traverse }
import org.scalatest.Matchers
import org.scalatest.funsuite.AnyFunSuiteLike

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.math.Numeric

class SectionSevenTestSuite extends AnyFunSuiteLike with Matchers {
  test("7.1.1 Folds and Folding") {
    def show[A](list: List[A]): String =
      list.foldLeft("nil")((accum, item) => s"$item then $accum")

    show(Nil) shouldBe "nil"
    show(List(1, 2, 3)) shouldBe "3 then 2 then 1 then nil"

    List(1, 2, 3).foldLeft(0)(_ + _) shouldBe 6
    List(1, 2, 3).foldRight(0)(_ + _) shouldBe 6

    List(1, 2, 3).foldLeft(0)(_ - _) shouldBe (0 - 1 - 2 - 3)
    List(1, 2, 3).foldRight(0)(_ - _) shouldBe (1 - (2 - (3 - 0)))
  }

  test("7.1.2 Exercise: Reflecting on Folds") {
    List(1, 2, 3).foldLeft(List.empty[Int])((a, i) => i :: a) shouldBe List(3, 2, 1)
    List(1, 2, 3).foldRight(List.empty[Int])((i, a) => i :: a) shouldBe List(1, 2, 3)
  }

  test("7.1.3 Exercise: Scaf-fold-ing Other Methods") {
    def map[A, B](list: List[A])(f: A => B): List[B] =
      list.foldRight(List.empty[B])((i, a) => f(i) :: a)

    map(List(1, 2, 3))(_ * 2) shouldBe List(2, 4, 6)

    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] =
      list.foldRight(List.empty[B])((i, a) => f(i) ++ a)

    flatMap(List(1, 2, 3))(a => List(a, a * 10, a * 100)) shouldBe List(1, 10, 100, 2, 20, 200, 3, 30, 300)

    def filter[A](list: List[A])(f: A => Boolean): List[A] =
      list.foldRight(List.empty[A])((i, a) => if (f(i)) i :: a else a)

    filter(List(1, 2, 3))(_ % 2 == 1) shouldBe List(1, 3)

    def sumWithNumeric[A](list: List[A])(implicit numeric: Numeric[A]): A =
      list.foldRight(numeric.zero)(numeric.plus)

    sumWithNumeric(List(1, 2, 3)) shouldBe 6

    def sumWithMonoid[A](list: List[A])(implicit monoid: Monoid[A]): A =
      list.foldRight(monoid.empty)(monoid.combine)

    sumWithMonoid(List(1, 2, 3)) shouldBe 6
  }

  test("7.1.4 Foldable in Cats") {
    val ints = List(1, 2, 3)

    Foldable[List].foldLeft(ints, 0)(_ + _) shouldBe 6

    val maybeInt = Option(123)

    Foldable[Option].foldLeft(maybeInt, 10)(_ * _) shouldBe 1230
  }

  test("7.1.4.2 Folding with Monoids") {
    Foldable[Option].nonEmpty(Option(42)) shouldBe true

    Foldable[List].find(List(1, 2, 3))(_ % 2 == 0) shouldBe Some(2)

    Foldable[List].combineAll(List(1, 2, 3)) shouldBe 6

    Foldable[List].foldMap(List(1, 2, 3))(_.toString) shouldBe "123"

    val ints = List(Vector(1, 2, 3), Vector(4, 5, 6))

    (Foldable[List] compose Foldable[Vector]).combineAll(ints) shouldBe 21
  }

  test("7.1.4.3 Syntax for Foldable") {
    List(1, 2, 3).combineAll shouldBe 6

    List(1, 2, 3).foldMap(_.toString) shouldBe "123"
  }

  test("7.2.1 Traversing with Futures") {
    val hostnames = List(
      "alpha.example.com",
      "beta.example.com",
      "gamma.demo.com"
    )

    def getUptime(hostname: String): Future[Int] = Future(hostname.length * 60)

    val allUptimes: Future[List[Int]] =
      hostnames.foldLeft(Future(List.empty[Int])) { (accum, host) =>
        val uptime = getUptime(host)
        for {
          accum  <- accum
          uptime <- uptime
        } yield accum :+ uptime
      }

    Await.result(allUptimes, 1.second) shouldBe List(1020, 960, 840)

    val allUptimes2: Future[List[Int]] =
      Future.traverse(hostnames)(getUptime)

    Await.result(allUptimes2, 1.second) shouldBe List(1020, 960, 840)

    def listTraverse[F[_]: Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
      list.foldLeft(List.empty[B].pure[F])((accum, item) => (accum, func(item)).mapN(_ :+ _))

    def listSequence[F[_]: Applicative, B](list: List[F[B]]): F[List[B]] =
      listTraverse(list)(identity)

    val totalUptime = listTraverse(hostnames)(getUptime)

    Await.result(totalUptime, 1.second) shouldBe List(1020, 960, 840)

    listSequence(List(Vector(1, 2), Vector(3, 4))) shouldBe Vector(List(1, 3), List(1, 4), List(2, 3), List(2, 4))

    listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6))) shouldBe Vector(
      List(1, 3, 5),
      List(1, 3, 6),
      List(1, 4, 5),
      List(1, 4, 6),
      List(2, 3, 5),
      List(2, 3, 6),
      List(2, 4, 5),
      List(2, 4, 6)
    )

    def process(inputs: List[Int]) =
      listTraverse(inputs)(n => if (n % 2 == 0) Some(n) else None)

    process(List(2, 4, 6)) shouldBe Some(List(2, 4, 6))
    process(List(1, 2, 3)) shouldBe None

    type ErrorsOr[A] = Validated[List[String], A]

    def process2(inputs: List[Int]): ErrorsOr[List[Int]] =
      listTraverse(inputs) { n =>
        if (n % 2 == 0) {
          Validated.valid(n)
        } else {
          Validated.invalid(List(s"$n is not even"))
        }
      }

    process2(List(2, 4, 6)) shouldBe Valid(List(2, 4, 6))
    process2(List(1, 2, 3)) shouldBe Invalid(List("1 is not even", "3 is not even"))
  }

  test("7.2.3 Traverse in Cats") {
    val hostnames = List(
      "alpha.example.com",
      "beta.example.com",
      "gamma.demo.com"
    )

    def getUptime(hostname: String): Future[Int] = Future(hostname.length * 60)

    val totalUptime: Future[List[Int]] =
      Traverse[List].traverse(hostnames)(getUptime)

    Await.result(totalUptime, 1.second) shouldBe List(1020, 960, 840)

    val numbers = List(Future(1), Future(2), Future(3))

    val numbers2: Future[List[Int]] =
      Traverse[List].sequence(numbers)

    Await.result(numbers2, 1.second) shouldBe List(1, 2, 3)

    Await.result(hostnames.traverse(getUptime), 1.second) shouldBe List(1020, 960, 840)
    Await.result(numbers.sequence, 1.second) shouldBe List(1, 2, 3)
  }

}
