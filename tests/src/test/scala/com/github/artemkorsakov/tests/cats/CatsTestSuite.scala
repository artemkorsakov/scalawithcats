package com.github.artemkorsakov.tests.cats

import java.util.Date

import cats._
import cats.data.State._
import cats.data.{ Reader, State, Writer }
import cats.instances.function._
import cats.instances.future._
import cats.instances.int._
import cats.instances.list._
import cats.instances.map._
import cats.instances.option._
import cats.instances.string._
import cats.instances.tuple._
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.contravariant._
import cats.syntax.either._
import cats.syntax.eq._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.invariant._
import cats.syntax.option._
import cats.syntax.semigroup._
import cats.syntax.show._
import cats.syntax.writer._
import com.github.artemkorsakov.cats.EqInstances._
import com.github.artemkorsakov.cats.LoginError.{ LoginResult, User, UserNotFound }
import com.github.artemkorsakov.cats.MyReader._
import com.github.artemkorsakov.cats.MyWriter._
import com.github.artemkorsakov.cats.ShowInstances._
import com.github.artemkorsakov.cats.{ Cat, LoginError }
import com.github.artemkorsakov.monsemi.SuperAdderInstances._
import com.github.artemkorsakov.monsemi.{ Order, SuperAdder }
import org.scalatest.Matchers
import org.scalatest.funsuite.AnyFunSuiteLike

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._

class CatsTestSuite extends AnyFunSuiteLike with Matchers {
  test("test cats.Show") {
    123.show shouldBe "123"
    "abc".show shouldBe "abc"
    new Date().show should endWith("ms since the epoch.")
    Cat("Garfield", 5, "white").show shouldBe "Garfield is a 5 year-old white cat."
  }

  test("test cats.Eq") {
    val eqInt = Eq[Int]
    eqInt.eqv(123, 123) shouldBe true
    eqInt.eqv(123, 234) shouldBe false

    (123 eqv 123) shouldBe true
    (123 =!= 123) shouldBe false
    (123 neqv 123) shouldBe false

    (123 eqv 234) shouldBe false
    (123 =!= 234) shouldBe true
    (123 neqv 234) shouldBe true

    1.some eqv none[Int] shouldBe false
    1.some =!= none[Int] shouldBe true

    val x = new Date() // now
    x eqv x shouldBe true
    val y = new Date() // a bit later than now
    x eqv y shouldBe false

    val cat1       = Cat("Garfield", 38, "white")
    val cat2       = Cat("Heathcliff", 33, "orange and black")
    val cat3       = Cat("Heathcliff", 32, "orange and black")
    val cat4       = Cat("Heathcliff", 33, "black")
    val cat5       = Cat("Garfield", 33, "orange and black")
    val cat6       = Cat("Heathcliff", 33, "orange and black")
    val optionCat1 = cat1.some
    val optionCat2 = none[Cat]

    cat1 eqv cat1 shouldBe true
    cat1 eqv cat2 shouldBe false
    cat2 eqv cat3 shouldBe false
    cat2 eqv cat4 shouldBe false
    cat2 eqv cat5 shouldBe false
    cat2 eqv cat6 shouldBe true
    optionCat1 eqv optionCat2 shouldBe false
  }

  test("test cats.Monoid") {
    Monoid[String].combine("Hi ", "there") shouldBe "Hi there"
    Monoid[String].empty shouldBe ""
    Semigroup[String].combine("Hi ", "there") shouldBe "Hi there"
    Monoid[Int].combine(32, 10) shouldBe 42
    Monoid[Option[Int]].combine(Some(32), Some(10)) shouldBe Some(42)
    ("Hi " |+| "there" |+| Monoid[String].empty) shouldBe "Hi there"
    (1 |+| 2 |+| Monoid[Int].empty) shouldBe 3
  }

  test("test SuperAdder.add") {
    SuperAdder.add(List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) shouldBe 55
    SuperAdder.add(
      List(0.some, 1.some, None, 2.some, 3.some, 4.some, 5.some, None)
    ) shouldBe 15.some

    SuperAdder.add(
      List(Order(3.6, 17.5), Order(13.3, 22.5), Order(23.63, 33.0))
    ) shouldBe Order(40.53, 73.0)

    val map1 = Map("a" -> 1, "b" -> 2)
    val map2 = Map("b" -> 3, "d" -> 4)
    (map1 |+| map2) shouldBe Map("b" -> 5, "d" -> 4, "a" -> 1)

    val tuple1: (String, Int) = ("hello", 123)
    val tuple2: (String, Int) = ("world", 321)
    val tuple                 = tuple1 |+| tuple2
    tuple._1 shouldBe "helloworld"
    tuple._2 shouldBe 444

    SuperAdder.addAll(List(1, 2, 3)) shouldBe 6
    SuperAdder.addAll(List(None, Some(1), Some(2))) shouldBe Some(3)
  }

  test("test Functor Type") {
    val list1 = List(1, 2, 3)
    Functor[List].map(list1)(_ * 2) shouldBe List(2, 4, 6)
    Functor[List].map(list1)(_ / 2) shouldBe List(0, 1, 1)

    val option1 = Option(123)
    Functor[Option].map(option1)(_.toString) shouldBe Some("123")

    val func       = (x: Int) => x + 1
    val liftedFunc = Functor[Option].lift(func)
    liftedFunc(Option(1)) shouldBe Some(2)

    Functor[List].as(list1, "As") shouldBe List("As", "As", "As")

    val func1 = (a: Int) => a + 1
    val func2 = (a: Int) => a * 2
    val func3 = (a: Int) => s"$a!"
    val func4 = func1.map(func2).map(func3)

    func4(123) shouldBe "248!"

    def doMath[F[_]](start: F[Int])(implicit functor: Functor[F]): F[Int] =
      start.map(n => n + 1 * 2)
    doMath(Option(20)) shouldBe Some(22)
    doMath(List(1, 2, 3)) shouldBe List(3, 4, 5)
  }

  test("test Contravariant in Cats") {
    val showString = Show[String]

    val showSymbol = Contravariant[Show].contramap(showString)((sym: Symbol) => s"'${sym.name}'")

    showSymbol.show(Symbol("dave")) shouldBe "'dave'"

    showString
      .contramap[Symbol](sym => s"'${sym.name}'")
      .show(Symbol("dave")) shouldBe "'dave'"
  }

  test("test Invariant in Cats") {
    implicit val symbolMonoid: Monoid[Symbol] =
      Monoid[String].imap(Symbol.apply)(_.name)

    Monoid[Symbol].empty.toString() shouldBe "'"

    (Symbol("a") |+| Symbol("few") |+| Symbol("words"))
      .toString() shouldBe "'afewwords"
  }

  test("test Aside: Partial Unification") {
    val func1 = (x: Int) => x.toDouble
    val func2 = (y: Double) => y * 2

    val func3 = func1.map(func2)
    func3(5) shouldBe 10.0

    val either: Either[String, Int] = Right(123)
    either.map(_ + 1) shouldBe Right(124)

    val func3a: Int => Double =
      a => func2(func1(a))
    func3a(5) shouldBe 10.0

    val func3b: Int => Double =
      func2.compose(func1)
    func3b(5) shouldBe 10.0
  }

  test("test The Monad Type Class") {
    val opt1 = Monad[Option].pure(3)
    opt1 shouldBe Some(3)
    val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 2))
    opt2 shouldBe Some(5)
    val opt3 = Monad[Option].map(opt2)(a => 100 * a)
    opt3 shouldBe Some(500)

    val list1 = Monad[List].pure(3)
    list1 shouldBe List(3)
    val list2 = Monad[List].flatMap(List(1, 2, 3))(a => List(a, a * 10))
    list2 shouldBe List(1, 10, 2, 20, 3, 30)
    val list3 = Monad[List].map(list2)(a => a + 123)
    list3 shouldBe List(124, 133, 125, 143, 126, 153)

    Monad[Option].flatMap(Option(1))(a => Option(a * 2)) shouldBe Some(2)
    Monad[List].flatMap(List(1, 2, 3))(a => List(a, a * 10)) shouldBe List(1, 10, 2, 20, 3, 30)
    Monad[Vector].flatMap(Vector(1, 2, 3))(a => Vector(a, a * 10)) shouldBe Vector(1, 10, 2, 20, 3, 30)

    val fm     = Monad[Future]
    val future = fm.flatMap(fm.pure(1))(x => fm.pure(x + 2))
    Await.result(future, 1.second) shouldBe 3
  }

  test("test Monad Syntax") {
    1.pure[Option] shouldBe Some(1)
    1.pure[List] shouldBe List(1)

    def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
      a.flatMap(x => b.map(y => x * x + y * y))

    sumSquare(Option(3), Option(4)) shouldBe Option(25)
    sumSquare(List(1, 2, 3), List(4, 5)) shouldBe List(17, 26, 20, 29, 25, 34)

    def sumSquare2[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
      for {
        x <- a
        y <- b
      } yield x * x + y * y

    sumSquare2(Option(3), Option(4)) shouldBe Option(25)
    sumSquare2(List(1, 2, 3), List(4, 5)) shouldBe List(17, 26, 20, 29, 25, 34)

    sumSquare(3: Id[Int], 4: Id[Int]) shouldBe 25

    ("Dave": Id[String]) shouldBe "Dave"
    (123: Id[Int]) shouldBe 123
    (List(1, 2, 3): Id[List[Int]]) shouldBe List(1, 2, 3)

    val a = Monad[Id].pure(3)
    a shouldBe 3
    val b = Monad[Id].flatMap(a)(_ + 1)
    b shouldBe 4
    (for {
      x <- a
      y <- b
    } yield x + y) shouldBe 7
  }

  test("test 4.4.2 Creating Instances") {
    val a = 3.asRight[String]
    a shouldBe Right(3)
    val b = 4.asRight[String]
    b shouldBe Right(4)

    val c = for {
      x <- a
      y <- b
    } yield x * x + y * y
    c shouldBe Right(25)

    def countPositive(nums: List[Int]) =
      nums.foldLeft(0.asRight[String]) { (accumulator, num) =>
        if (num > 0) {
          accumulator.map(_ + 1)
        } else {
          Left("Negative. Stopping!")
        }
      }

    countPositive(List(1, 2, 3)) shouldBe Right(3)
    countPositive(List(1, -2, 3)) shouldBe Left("Negative. Stopping!")

    println(Either.catchOnly[NumberFormatException]("foo".toInt))
    println(Either.catchNonFatal(sys.error("Badness")))
    println(Either.fromTry(scala.util.Try("foo".toInt)))
    println(Either.fromOption[String, Int](None, "Badness"))
  }

  test("test 4.4.3 Transforming Eithers") {
    "Error".asLeft[Int].getOrElse(0) shouldBe 0
    "Error".asLeft[Int].orElse(2.asRight[String]) shouldBe Right(2)
    // Either[String, Int] = Right(2)

    (-1).asRight[String].ensure("Must be non-negative!")(_ > 0) shouldBe Left("Must be non-negative!")
    // Either[String, Int] = Left("Must be non-negative!")

    "error".asLeft[Int].recover {
      case _: String => -1
    } shouldBe Right(-1)
    // Either[String, Int] = Right(-1)

    "error".asLeft[Int].recoverWith {
      case _: String => Right(-1)
    } shouldBe Right(-1)
    // Either[String, Int] = Right(-1)

    "foo".asLeft[Int].leftMap(_.reverse) shouldBe Left("oof")
    // Either[String, Int] = Left("oof")
    6.asRight[String].bimap(_.reverse, _ * 7) shouldBe Right(42)
    // Either[String, Int] = Right(42)
    "bar".asLeft[Int].bimap(_.reverse, _ * 7) shouldBe Left("rab")
    // Either[String, Int] = Left("rab")

    123.asRight[String] shouldBe Right(123)
    // Either[String, Int] = Right(123)
    123.asRight[String].swap shouldBe Left(123)
    // Either[Int, String] = Left(123)
  }

  test("test 4.4.4 Error Handling") {
    (for {
      a <- 1.asRight[String]
      b <- 0.asRight[String]
      c <- if (b == 0) "DIV0".asLeft[Int]
      else (a / b).asRight[String]
    } yield c * 100) shouldBe Left("DIV0")
    // Either[String, Int] = Left("DIV0")

    val result1: LoginResult = User("dave", "passw0rd").asRight
    result1 shouldBe Right(User("dave", "passw0rd"))
    val result2: LoginResult = UserNotFound("dave").asLeft
    result2 shouldBe Left(UserNotFound("dave"))

    result1.fold(LoginError.handleError, println)
    // User(dave,passw0rd)
    result2.fold(LoginError.handleError, println)
    // User not found: dave
  }

  test("test 4.6.1 Eager, Lazy, Memoized, Oh My!") {
    val now = Eval.now(math.random + 1000)
    println(now)
    // now: Eval[Double] = Now(1000.020590704322)
    val always = Eval.always(math.random + 3000)
    println(always)
    // always: Eval[Double] = cats.Always@4d8ca6eb
    val later = Eval.later(math.random + 2000)
    println(later)
    // later: Eval[Double] = cats.Later@601dc0b2

    println(now.value)
    // res6: Double = 1000.020590704322
    println(always.value)
    // res7: Double = 3000.97102818157
    println(later.value)
    // res8: Double = 2000.0126977436273

    val x = Eval.now {
      println("Computing X")
      math.random
    }
    // Computing X
    // x: Eval[Double] = Now(0.681816469770503)

    println(x.value) // first access
    // res10: Double = 0.681816469770503 // first access
    println(x.value) // second access
    // res11: Double = 0.681816469770503

    val y = Eval.always {
      println("Computing Y")
      math.random
    }
    // y: Eval[Double] = cats.Always@414a351

    println(y.value) // first access
    // Computing Y
    // res12: Double = 0.09982997820703643 // first access
    println(y.value) // second access
    // Computing Y
    // res13: Double = 0.34240334819463436

    val z = Eval.later {
      println("Computing Z")
      math.random
    }
    // z: Eval[Double] = cats.Later@b0a344a

    println(z.value) // first access
    // Computing Z
    // res14: Double = 0.3604236919233441 // first access
    println(z.value) // second access
    // res15: Double = 0.3604236919233441

    val greeting = Eval
      .always { println("Step 1"); "Hello" }
      .map { str => println("Step 2"); s"$str world" }

    println(greeting.value)

    val ans = for {
      a <- Eval.now { println("Calculating A"); 40 }
      b <- Eval.always { println("Calculating B"); 2 }
    } yield {
      println("Adding A and B")
      a + b
    }

    println(ans.value)
    println(ans.value)

    val saying = Eval
      .always { println("Step 1"); "The cat" }
      .map { str => println("Step 2"); s"$str sat on" }
      .memoize
      .map { str => println("Step 3"); s"$str the mat" }
    // saying: Eval[String] = cats.Eval$$anon$4@ca01c64

    println(saying.value) // first access
    // Step 1
    // Step 2
    // Step 3
    // res19: String = "The cat sat on the mat" // first access
    println(saying.value) // second access
    // Step 3
    // res20: String = "The cat sat on the mat"
  }

  test("test 4.6.4 Trampolining and Eval.defer") {
    def factorial(n: BigInt): Eval[BigInt] =
      if (n == 1) {
        Eval.now(n)
      } else {
        Eval.defer(factorial(n - 1).map(_ * n))
      }

    factorial(50000).value
  }

  test("test 4.6.5 Exercise: Safer Folding using Eval") {
    def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
      as match {
        case head :: tail =>
          fn(head, foldRight(tail, acc)(fn))
        case Nil =>
          acc
      }

    val res = foldRight((1 to 10).toList, "start")((x, y) => y + " - " + x)
    res shouldBe "start - 10 - 9 - 8 - 7 - 6 - 5 - 4 - 3 - 2 - 1"

    def foldRight2[A, B](as: List[A], acc: B)(fn: (A, B) => B): Eval[B] =
      as match {
        case head :: tail =>
          Eval.later(fn(head, Eval.defer(foldRight2(tail, acc)(fn)).value))
        case Nil =>
          Eval.now(acc)
      }

    val res2 = foldRight2((1 to 10).toList, "start")((x, y) => y + " - " + x)
    res2.value shouldBe "start - 10 - 9 - 8 - 7 - 6 - 5 - 4 - 3 - 2 - 1"

    def foldRightEval[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
      as match {
        case head :: tail =>
          Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
        case Nil =>
          acc
      }

    def foldRight3[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
      foldRightEval(as, Eval.now(acc))((a, b) => b.map(fn(a, _))).value

    foldRight3((1 to 100000).toList, 0L)(_ + _) shouldBe 5000050000L
  }

  test("test 4.7.1 Creating and Unpacking Writers") {
    Writer(
      Vector(
        "It was the best of times",
        "it was the worst of times"
      ),
      1859
    )

    type Logged[A] = Writer[Vector[String], A]
    println(123.pure[Logged])

    println(Vector("msg1", "msg2", "msg3").tell)

    val a = Writer(Vector("msg1", "msg2", "msg3"), 123)
    println(a)
    // a: cats.data.WriterT[cats.package.Id, Vector[String], Int] = WriterT(
    //   (Vector("msg1", "msg2", "msg3"), 123)
    // )
    val b = 123.writer(Vector("msg1", "msg2", "msg3"))
    println(b)
    // b: Writer[Vector[String], Int] = WriterT(
    //   (Vector("msg1", "msg2", "msg3"), 123)
    // )

    val aResult: Int = a.value
    aResult shouldBe 123
    // aResult: Int = 123
    val aLog: Vector[String] = a.written
    aLog shouldBe Vector("msg1", "msg2", "msg3")
    // aLog: Vector[String] = Vector("msg1", "msg2", "msg3")

    val (log, result) = b.run
    log shouldBe Vector("msg1", "msg2", "msg3")
    result shouldBe 123
    // log: Vector[String] = Vector("msg1", "msg2", "msg3")
    // result: Int = 123

    val writer1 = for {
      a <- 10.pure[Logged]
      _ <- Vector("a", "b", "c").tell
      b <- 32.writer(Vector("x", "y", "z"))
    } yield a + b
    // writer1: cats.data.WriterT[cats.package.Id, Vector[String], Int] = WriterT(
    //   (Vector("a", "b", "c", "x", "y", "z"), 42)
    // )

    println(writer1.run)
    // res3: (Vector[String], Int) = (Vector("a", "b", "c", "x", "y", "z"), 42)

    val writer2 = writer1.mapWritten(_.map(_.toUpperCase))
    // writer2: cats.data.WriterT[cats.package.Id, Vector[String], Int] = WriterT(
    //   (Vector("A", "B", "C", "X", "Y", "Z"), 42)
    // )

    println(writer2.run)
    // res4: (Vector[String], Int) = (Vector("A", "B", "C", "X", "Y", "Z"), 42)

    val writer3 = writer1.bimap(
      log => log.map(_.toUpperCase),
      res => res * 100
    )
    // writer3: cats.data.WriterT[cats.package.Id, Vector[String], Int] = WriterT(
    //   (Vector("A", "B", "C", "X", "Y", "Z"), 4200)
    // )

    println(writer3.run)
    // res5: (Vector[String], Int) = (Vector("A", "B", "C", "X", "Y", "Z"), 4200)

    val writer4 = writer1.mapBoth { (log, res) =>
      val log2 = log.map(_ + "!")
      val res2 = res * 1000
      (log2, res2)
    }
    // writer4: cats.data.WriterT[cats.package.Id, Vector[String], Int] = WriterT(
    //   (Vector("a!", "b!", "c!", "x!", "y!", "z!"), 42000)
    // )

    println(writer4.run)
    // res6: (Vector[String], Int) = (
    //   Vector("a!", "b!", "c!", "x!", "y!", "z!"),
    //   42000
    // )

    val writer5 = writer1.reset
    // writer5: cats.data.WriterT[cats.package.Id, Vector[String], Int] = WriterT(
    //   (Vector(), 42)
    // )

    println(writer5.run)
    // res7: (Vector[String], Int) = (Vector(), 42)

    val writer6 = writer1.swap
    // writer6: cats.data.WriterT[cats.package.Id, Int, Vector[String]] = WriterT(
    //   (42, Vector("a", "b", "c", "x", "y", "z"))
    // )

    println(writer6.run)
    // res8: (Int, Vector[String]) = (42, Vector("a", "b", "c", "x", "y", "z"))
  }

  test("test 4.7.3 Exercise: Show Your Working") {
    val res = Await.result(
      Future.sequence(
        Vector(
          Future(factorial(5)),
          Future(factorial(6))
        )
      ),
      5.seconds
    )
    println(res.head)
    println(res.last)
  }

  test("test 4.8 The Reader Monad") {
    final case class Cat(name: String, favoriteFood: String)

    val catName: Reader[Cat, String] = Reader(cat => cat.name)
    catName.run(Cat("Garfield", "lasagne")) shouldBe "Garfield"

    val greetKitty: Reader[Cat, String] = catName.map(name => s"Hello $name")
    greetKitty.run(Cat("Heathcliff", "junk food")) shouldBe "Hello Heathcliff"

    val feedKitty: Reader[Cat, String] =
      Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")

    val greetAndFeed: Reader[Cat, String] =
      for {
        greet <- greetKitty
        feed  <- feedKitty
      } yield s"$greet. $feed."

    greetAndFeed(Cat("Garfield", "lasagne")) shouldBe "Hello Garfield. Have a nice bowl of lasagne."
    greetAndFeed(Cat("Heathcliff", "junk food")) shouldBe "Hello Heathcliff. Have a nice bowl of junk food."
  }

  test("test 4.8.3 Exercise: Hacking on Readers") {
    val users = Map(
      1 -> "dade",
      2 -> "kate",
      3 -> "margo"
    )

    val passwords = Map(
      "dade"  -> "zerocool",
      "kate"  -> "acidburn",
      "margo" -> "secret"
    )

    val db = Db(users, passwords)

    findUsername(1).run(db) shouldBe Some("dade")
    findUsername(4).run(db) shouldBe None

    checkPassword("dade", "zerocool").run(db) shouldBe true
    checkPassword("kate", "zerocool").run(db) shouldBe false
    checkPassword("dade", "zerocoo").run(db) shouldBe false
    checkPassword("dade", "secret").run(db) shouldBe false
    checkPassword("dade1", "zerocool").run(db) shouldBe false

    checkLogin(1, "zerocool").run(db) shouldBe true
    checkLogin(4, "davinci").run(db) shouldBe false
  }

  test("4.9.1 Creating and Unpacking State") {
    val a = State[Int, String](state => (state, s"The state is $state"))

    // Get the state and the result:
    val (state, result) = a.run(10).value
    state shouldBe 10
    result shouldBe "The state is 10"
    // state: Int = 10
    // result: String = "The state is 10"

    // Get the state, ignore the result:
    val justTheState = a.runS(10).value
    justTheState shouldBe 10
    // justTheState: Int = 10

    // Get the result, ignore the state:
    val justTheResult = a.runA(10).value
    justTheResult shouldBe "The state is 10"
    // justTheResult: String = "The state is 10"
  }

  test("4.9.2 Composing and Transforming State") {
    val step1 = State[Int, String] { num =>
      val ans = num + 1
      (ans, s"Result of step1: $ans")
    }

    val step2 = State[Int, String] { num =>
      val ans = num * 2
      (ans, s"Result of step2: $ans")
    }

    val both = for {
      a <- step1
      b <- step2
    } yield (a, b)

    val (stateTmp, resultTmp) = both.run(20).value
    stateTmp shouldBe 42
    resultTmp shouldBe (("Result of step1: 21", "Result of step2: 42"))
    // state: Int = 42
    // result: (String, String) = ("Result of step1: 21", "Result of step2: 42")

    val getDemo = State.get[Int]
    // getDemo: State[Int, Int] = cats.data.IndexedStateT@741518c8
    getDemo.run(10).value shouldBe (10 -> 10)
    // res1: (Int, Int) = (10, 10)

    val setDemo = State.set[Int](30)
    // setDemo: State[Int, Unit] = cats.data.IndexedStateT@509fb0a
    setDemo.run(10).value shouldBe ((30, ()))
    // res2: (Int, Unit) = (30, ())

    val pureDemo = State.pure[Int, String]("Result")
    // pureDemo: State[Int, String] = cats.data.IndexedStateT@562ae0a8
    pureDemo.run(10).value shouldBe (10 -> "Result")
    // res3: (Int, String) = (10, "Result")

    val inspectDemo = State.inspect[Int, String](x => s"$x!")
    // inspectDemo: State[Int, String] = cats.data.IndexedStateT@2dc6b50f
    inspectDemo.run(10).value shouldBe (10 -> "10!")
    // res4: (Int, String) = (10, "10!")

    val modifyDemo = State.modify[Int](_ + 1)
    // modifyDemo: State[Int, Unit] = cats.data.IndexedStateT@71c93b27
    modifyDemo.run(10).value shouldBe ((11, ()))
    // res5: (Int, Unit) = (11, ())

    val program: State[Int, (Int, Int, Int)] = for {
      a <- get[Int]
      _ <- set[Int](a + 1)
      b <- get[Int]
      _ <- modify[Int](_ + 1)
      c <- inspect[Int, Int](_ * 1000)
    } yield (a, b, c)
    // program: State[Int, (Int, Int, Int)] = cats.data.IndexedStateT@3b525fbf

    val (state, result) = program.run(1).value
    state shouldBe 3
    result shouldBe ((1, 2, 3000))
    // state: Int = 3
    // result: (Int, Int, Int) = (1, 2, 3000)
  }

  test("4.9.3 Exercise: Post-Order Calculator") {}
}
