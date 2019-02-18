package com.example
package presentation

import cats.effect._

object IxFreeExample {

  trait LoggedOut
  trait LoggedIn

  sealed trait UserOp[F, T, A]
  case class Login(c: Credentials) extends UserOp[LoggedOut, LoggedIn, Unit]
  case object ReadSecret extends UserOp[LoggedIn, LoggedIn, String]
  case object Logout extends UserOp[LoggedIn, LoggedOut, Unit]

  object UserApi {
    def login(c: Credentials): IxFree[UserOp, LoggedOut, LoggedIn, Unit] =
      IxFree.liftF(Login(c))
    def readSecret: IxFree[UserOp, LoggedIn, LoggedIn, String] =
      IxFree.liftF(ReadSecret)
    def logout: IxFree[UserOp, LoggedIn, LoggedOut, Unit] =
      IxFree.liftF(Logout)
  }

  val interpreter: FunctionX[UserOp, ({ type λ[f, t, A] = IO[A] })#λ] = ???

  val myProgram: IxFree[UserOp, LoggedOut, LoggedOut, String] = for {
    _ <- UserApi.login(myCredentials)
    secret <- UserApi.readSecret
    _ <- UserApi.logout
  } yield secret

  val myIO: IO[String] =
    myProgram.foldMap[({ type λ[f, t, A] = IO[A] })#λ](interpreter)

  val badProgram: IxFree[UserOp, LoggedIn, LoggedOut, String] = for {
    secret <- UserApi.readSecret
    _ <- UserApi.logout
  } yield secret

  val badIO: IO[String] =
    badProgram.foldMap[({ type λ[f, t, A] = IO[A] })#λ](interpreter)
}
