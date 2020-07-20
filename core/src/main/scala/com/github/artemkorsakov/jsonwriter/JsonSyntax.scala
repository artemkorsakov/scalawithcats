package com.github.artemkorsakov.jsonwriter

object JsonSyntax {
  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit writer: JsonWriter[A]): Json =
      writer.write(value)
  }

  implicit def optionWriter[A](
    implicit writer: JsonWriter[A]
  ): JsonWriter[Option[A]] = {
    case Some(aValue) => writer.write(aValue)
    case None         => JsNull
  }
}
