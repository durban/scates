/*
 * Copyright 2019 Daniel Urban and contributors listed in AUTHORS
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
