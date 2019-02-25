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
