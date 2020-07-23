package com.github.artemkorsakov.monads

object MyId extends {
  type MyId[A] = A

  def pure[A](a: A): MyId[A] = a

  def flatMap[A, B](value: MyId[A])(func: A => MyId[B]): MyId[B] = func(value)

  def map[A, B](value: MyId[A])(func: A => B): MyId[B] = func(value)
}
