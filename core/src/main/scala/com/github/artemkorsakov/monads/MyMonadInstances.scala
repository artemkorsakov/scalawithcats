package com.github.artemkorsakov.monads

object MyMonadInstances {
  implicit val listMonad: MyMonad[List] =
    new MyMonad[List] {
      override def pure[A](a: A): List[A] = List(a)
      override def flatMap[A, B](value: List[A])(func: A => List[B]): List[B] =
        value.flatMap(x => func(x))
    }
}
