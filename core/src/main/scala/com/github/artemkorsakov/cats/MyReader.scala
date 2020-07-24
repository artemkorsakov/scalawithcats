package com.github.artemkorsakov.cats

import cats.data.Reader
import cats.syntax.applicative._

object MyReader {
  final case class Db(
      usernames: Map[Int, String],
      passwords: Map[String, String]
  )

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db => db.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(db => db.passwords.getOrElse(username, "").equals(password))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
    for {
      username <- findUsername(userId)
      passwordOk <- username.map(username => checkPassword(username, password)).getOrElse {
        false.pure[DbReader]
      }
    } yield passwordOk
}
