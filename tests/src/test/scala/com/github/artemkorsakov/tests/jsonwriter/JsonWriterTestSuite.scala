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

    val str3 = Json.toJson(Option("Dave"))
    str3 shouldBe JsString("Dave")

    val noneStr: Option[String] = None
    val str4 = Json.toJson(noneStr)
    str4 shouldBe JsNull

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

    val person3 = Json.toJson(Option(Person("Dave", "dave@example.com")))
    person3 shouldBe JsObject(Map(
      "name" -> JsString("Dave"),
      "email" -> JsString("dave@example.com")
    ))

    val nonePerson: Option[Person] = None
    val person4 = Json.toJson(nonePerson)
    person4 shouldBe JsNull
  }
}
