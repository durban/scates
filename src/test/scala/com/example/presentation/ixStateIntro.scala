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

import cats.Id
import cats.data.{ State, IndexedStateT }

object StateIntro {

  def mkState[S, A](run: S ⇒ (S, A)): cats.data.State[S, A] =
    cats.data.State(run)

  // (Simple) State monad:
  case class State[S, A](run: S ⇒ (S, A))
}

object StateIntroExample {

  val simpleProg: State[Int, Unit] = for {
    num ← State.get // read an Int
    _ ← State.set(num + 1) // write an Int
  } yield ()
}

object IxStateIntro {

  def mkIxState[F, T, A](run: F ⇒ (T, A)): cats.data.IndexedStateT[Id, F, T, A] =
    cats.data.IndexedStateT[Id, F, T, A](run)

  // Indexed State monad:
  case class IndexedState[F, T, A](run: F ⇒ (T, A))
}

object IxStateIntroExample {

  type IndexedState[F, T, A] = IndexedStateT[Id, F, T, A]

  object IndexedState {
    def get[SA]: IndexedState[SA, SA, SA] =
      IndexedStateT.get[Id, SA]
    def set[SA, SB](sb: SB): IndexedState[SA, SB, Unit] =
      IndexedStateT.set(sb)
  }

  val indexedProg: IndexedState[Int, String, Unit] = for {
    num ← IndexedState.get // read an Int
    _ ← IndexedState.set("str") // write a String!
  } yield ()
}
