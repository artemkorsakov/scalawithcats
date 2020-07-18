package com.github.artemkorsakov.printable

object PrintableInstances {
  implicit val stringPrintable: Printable[String] =
    (value: String) => s"Print string $value"

  implicit val intPrintable: Printable[Int] =
    (value: Int) => s"Print int $value"
}
