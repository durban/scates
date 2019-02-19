package com.example
package presentation

import cats.~>
import cats.free.Free
import cats.effect._

object FreeExample {

  sealed trait UserOp[A]
  case class Login(c: Credentials) extends UserOp[Unit]
  case object ReadSecret extends UserOp[String]
  case object Logout extends UserOp[Unit]

  object UserApi extends UserApi {
    def login(c: Credentials): Free[UserOp, Unit] =
      Free.liftF(Login(c))
    def readSecret: Free[UserOp, String] =
      Free.liftF(ReadSecret)
    def logout: Free[UserOp, Unit] =
      Free.liftF(Logout)
  }

  trait UserApi {
    def login(c: Credentials): Free[UserOp, Unit]
    def readSecret: Free[UserOp, String]
    def logout: Free[UserOp, Unit]
  }

  val myProgram: Free[UserOp, String] = for {
    _ ← UserApi.login(myCredentials)
    secret ← UserApi.readSecret
    _ ← UserApi.logout
  } yield secret

  val interpreter: UserOp ~> IO = ??? // ...

  val myIO: IO[String] =
    myProgram.foldMap[IO](interpreter)

  val badProgram: Free[UserOp, String] = for {
    secret ← UserApi.readSecret
    // login AFTER readSecret
    _ ← UserApi.login(myCredentials)
  } yield secret

  val badIO: IO[String] =
    badProgram.foldMap[IO](interpreter)
}
