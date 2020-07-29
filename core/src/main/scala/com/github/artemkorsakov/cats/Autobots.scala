package com.github.artemkorsakov.cats

import cats.data.EitherT
import cats.instances.future._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ Await, Future }
import scala.concurrent.duration._

object Autobots {
  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz"      -> 6,
    "Bumblebee" -> 8,
    "Hot Rod"   -> 10
  )

  def getPowerLevel(ally: String): Response[Int] =
    powerLevels.get(ally) match {
      case Some(avg) => EitherT.right(Future(avg))
      case None      => EitherT.left(Future(s"$ally unreachable"))
    }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
    for {
      a <- getPowerLevel(ally1)
      b <- getPowerLevel(ally2)
    } yield a + b > 15

  def tacticalReport(ally1: String, ally2: String): String = {
    val stack = canSpecialMove(ally1, ally2).value

    Await.result(stack, 1.second) match {
      case Left(msg) =>
        s"Comms error: $msg"
      case Right(true) =>
        s"$ally1 and $ally2 are ready to roll out!"
      case Right(false) =>
        s"$ally1 and $ally2 need a recharge."
    }
  }

}
