package com.github.artemkorsakov.cats

import java.util.Date

import cats.Show
import cats.instances.int._
import cats.instances.string._
import cats.syntax.show._

final case class Cat(name: String, age: Int, color: String)

object ShowInstances {
  implicit val dateShow: Show[Date] =
    Show.show(date => s"${date.getTime}ms since the epoch.")

  implicit val catPrintable: Show[Cat] =
    Show.show(cat => s"${cat.name.show} is a ${cat.age.show} year-old ${cat.color.show} cat.")
}
