package com.github.artemkorsakov.jsonwriter

final case class Person(name: String, email: String)

object JsonWriterInstances {
  implicit val stringWriter: JsonWriter[String] =
    (value: String) => JsString(value)

  implicit val personWriter: JsonWriter[Person] =
    (value: Person) =>
      JsObject(
        Map("name" -> JsString(value.name), "email" -> JsString(value.email))
    )

}
