package com.example
package presentation

import cats.data.IndexedStateT
import cats.effect._

object IxStateExample {

  object UserApi extends UserApi {
    def login(c: Credentials): IndexedStateT[IO, LoggedOut, LoggedIn, Unit] = ???
    def readSecret: IndexedStateT[IO, LoggedIn, LoggedIn, String] = ???
    def logout: IndexedStateT[IO, LoggedIn, LoggedOut, Unit] = ???
  }

  class LoggedOut; class LoggedIn
  trait UserApi {
    def login(c: Credentials):
      IndexedStateT[IO, LoggedOut, LoggedIn, Unit]
    def readSecret:
      IndexedStateT[IO, LoggedIn, LoggedIn, String]
    def logout: IndexedStateT[IO, LoggedIn, LoggedOut, Unit]
  }
  val myProg: IndexedStateT[IO, LoggedOut, LoggedOut, String] =
    for {
      _ ← UserApi.login(myCredentials)
      secret ← UserApi.readSecret
      _ ← UserApi.logout
    } yield secret

  val myIO: IO[String] =
    myProg.runA(new LoggedOut)

  val prog2: IndexedStateT[IO, LoggedOut, LoggedOut, String] = {
  for {
    _ ← IndexedStateT.set[IO, LoggedOut, LoggedIn](
      new LoggedIn) // invalid operation!
    secret ← UserApi.readSecret
    _ ← UserApi.logout
  } yield secret
  }

  val badIO: IO[String] =
    prog2.runA(new LoggedOut)
}
