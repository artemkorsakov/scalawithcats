package com.github.artemkorsakov.tests.printable

import com.github.artemkorsakov.printable.CodecInstances._
import com.github.artemkorsakov.printable._
import org.scalatest.Matchers
import org.scalatest.funsuite.AnyFunSuiteLike

class CodecTestSuite extends AnyFunSuiteLike with Matchers {
  test("test Codec") {
    Codec.encode("encode") shouldBe "encode"
    Codec.decode[String]("decode") shouldBe "decode"

    Codec.encode(123) shouldBe "123"
    Codec.decode[Int]("123") shouldBe 123

    Codec.encode(true) shouldBe "true"
    Codec.decode[Boolean]("true") shouldBe true

    Codec.encode(123.4) shouldBe "123.4"
    Codec.decode[Double]("123.4") shouldBe 123.4

    Codec.encode(Box("encode")) shouldBe "encode"
    Codec.decode[Box[String]]("decode") shouldBe Box("decode")

    Codec.encode(Box(123)) shouldBe "123"
    Codec.decode[Box[Int]]("123") shouldBe Box(123)

    Codec.encode(Box(true)) shouldBe "true"
    Codec.decode[Box[Boolean]]("true") shouldBe Box(true)

    Codec.encode(Box(123.4)) shouldBe "123.4"
    Codec.decode[Box[Double]]("123.4") shouldBe Box(123.4)
  }
}
