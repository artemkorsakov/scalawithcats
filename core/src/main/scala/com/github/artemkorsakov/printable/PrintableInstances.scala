package com.github.artemkorsakov.printable

final case class Cat(name: String, age: Int, color: String)

object PrintableInstances {
  implicit val stringPrintable: Printable[String] =
    (value: String) => value

  implicit val intPrintable: Printable[Int] =
    (value: Int) => value.toString

  implicit val catPrintable: Printable[Cat] =
    (cat: Cat) => {
      val name = Printable.format(cat.name)
      val age = Printable.format(cat.age)
      val color = Printable.format(cat.color)
      s"$name is a $age year-old $color cat."
    }
}
