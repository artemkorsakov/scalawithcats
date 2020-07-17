package com.github.artemkorsakov.tests.jsonwriter

import com.github.artemkorsakov.jsonwriter.JsonSyntax._
import com.github.artemkorsakov.jsonwriter.JsonWriterInstances._
import com.github.artemkorsakov.jsonwriter._
import org.scalatest.Matchers
import org.scalatest.funsuite.AnyFunSuiteLike

class JsonWriterTestSuite extends AnyFunSuiteLike with Matchers {
  test("test JsonWriterInstances") {
    val str1 = Json.toJson("Dave")
    str1 shouldBe JsString("Dave")

    val str2 = "Dave".toJson
    str2 shouldBe JsString("Dave")

    val person1 = Json.toJson(Person("Dave", "dave@example.com"))
    person1 shouldBe JsObject(Map(
      "name" -> JsString("Dave"),
      "email" -> JsString("dave@example.com")
    ))

    val person2 = Person("Dave", "dave@example.com").toJson
    person2 shouldBe JsObject(Map(
      "name" -> JsString("Dave"),
      "email" -> JsString("dave@example.com")
    ))

    println(implicitly[JsonWriter[String]])
    println(implicitly[JsonWriter[Person]])
  }
}
