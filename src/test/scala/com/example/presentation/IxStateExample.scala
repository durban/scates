package com.example
package presentation

import cats.data.IndexedStateT
import cats.effect._

object IxStateExample {

  final class LoggedOut
  final class LoggedIn

  object UserApi {
    def login(c: Credentials): IndexedStateT[IO, LoggedOut, LoggedIn, Unit] = ???
    def readSecret: IndexedStateT[IO, LoggedIn, LoggedIn, String] = ???
    def logout: IndexedStateT[IO, LoggedIn, LoggedOut, Unit] = ???
  }

  val myProgram: IndexedStateT[IO, LoggedOut, LoggedOut, String] = for {
    _ <- UserApi.login(myCredentials)
    secret <- UserApi.readSecret
    _ <- UserApi.logout
  } yield secret

  val myIO: IO[String] =
    myProgram.runA(new LoggedOut)

  val badProgram: IndexedStateT[IO, LoggedOut, LoggedOut, String] = for {
    _ <- IndexedStateT.set[IO, LoggedOut, LoggedIn](new LoggedIn) // Fake login!
    secret <- UserApi.readSecret
    _ <- UserApi.logout
  } yield secret

  val badIO: IO[String] =
    badProgram.runA(new LoggedOut)
}
