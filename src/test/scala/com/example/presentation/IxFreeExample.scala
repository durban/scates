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

  object UserApi extends UserApi {
    def login(c: Credentials): IxFree[UserOp, LoggedOut, LoggedIn, Unit] =
      IxFree.liftF(Login(c))
    def readSecret: IxFree[UserOp, LoggedIn, LoggedIn, String] =
      IxFree.liftF(ReadSecret)
    def logout: IxFree[UserOp, LoggedIn, LoggedOut, Unit] =
      IxFree.liftF(Logout)
  }

  val interpreter: FunctionX[UserOp, ({ type λ[f, t, A] = IO[A] })#λ] = ???

  trait UserApi {
    def login(c: Credentials):
      IxFree[UserOp, LoggedOut, LoggedIn, Unit]
    def readSecret:
      IxFree[UserOp, LoggedIn, LoggedIn, String]
    def logout: IxFree[UserOp, LoggedIn, LoggedOut, Unit]
  }

  val myProg: IxFree[UserOp, LoggedOut, LoggedOut, String] =
    for {
      // we need to actually `login`:
      _ ← UserApi.login(myCredentials)
      secret ← UserApi.readSecret
      _ ← UserApi.logout
    } yield secret

  val myIO: IO[String] =
    myProg.foldMapA(interpreter)
}
