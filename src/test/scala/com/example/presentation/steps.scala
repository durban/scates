package com.example
package presentation

import cats.Id
import cats.free.Free
import cats.data.IndexedStateT

trait Credentials

object FreeExample {

  sealed trait UserOp[A]
  case class Login(c: Credentials) extends UserOp[Unit]
  case object ReadSecret extends UserOp[String]
  case object Logout extends UserOp[Unit]

  object UserApi {
    def login(c: Credentials): Free[UserOp, Unit] =
      Free.liftF(Login(c))
    def readSecret: Free[UserOp, String] =
      Free.liftF(ReadSecret)
    def logout: Free[UserOp, Unit] =
      Free.liftF(Logout)
  }

  val myProgram: Free[UserOp, String] = for {
    _ <- UserApi.login(myCredentials)
    secret <- UserApi.readSecret
    _ <- UserApi.logout
  } yield secret

  val badProgram: Free[UserOp, String] = for {
    // No login!
    secret <- UserApi.readSecret
    _ <- UserApi.logout
  } yield secret
}

object IxStateExample {

  // FIXME: use IndexedStateT with IO, for real logic?

  type IndexedState[F, T, A] = IndexedStateT[Id, F, T, A]
  object IndexedState {
    def set[SA, SB](sb: SB): IndexedState[SA, SB, Unit] =
      IndexedStateT.set[Id, SA, SB](sb)
    def modify[SA, SB](f: SA => SB): IndexedState[SA, SB, Unit] =
      IndexedStateT.modify[Id, SA, SB](f)
  }

  trait LoggedOut
  trait LoggedIn

  object UserApi {
    def login(c: Credentials): IndexedState[LoggedOut, LoggedIn, Unit] = ???
    def readSecret: IndexedState[LoggedIn, LoggedIn, String] = ???
    def logout: IndexedState[LoggedIn, LoggedOut, Unit] = ???
  }

  val myProgram: IndexedState[LoggedOut, LoggedOut, String] = for {
    _ <- UserApi.login(myCredentials)
    secret <- UserApi.readSecret
    _ <- UserApi.logout
  } yield secret

  val badProgram: IndexedState[LoggedOut, LoggedOut, String] = for {
    _ <- IndexedState.set(new LoggedIn {}) // Fake login!
    secret <- UserApi.readSecret
    _ <- UserApi.logout
  } yield secret
}

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

  val myProgram: IxFree[UserOp, LoggedOut, LoggedOut, String] = for {
    _ <- UserApi.login(myCredentials)
    secret <- UserApi.readSecret
    _ <- UserApi.logout
  } yield secret
}
