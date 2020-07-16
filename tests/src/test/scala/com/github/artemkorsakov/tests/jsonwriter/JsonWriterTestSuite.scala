package com.github.artemkorsakov.tests.jsonwriter

import com.github.artemkorsakov.jsonwriter.JsonWriterInstances._
import com.github.artemkorsakov.jsonwriter.{Json, Person}
import org.scalatest.Matchers
import org.scalatest.funsuite.AnyFunSuiteLike

class JsonWriterTestSuite extends AnyFunSuiteLike with Matchers {
  test("test") {
    println(Json.toJson(Person("Dave", "dave@example.com")))
    1 shouldBe 1
  }
}
