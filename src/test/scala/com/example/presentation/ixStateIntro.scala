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
    num ← State.get
    _ ← State.set(num + 1)
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
    num ← IndexedState.get
    _ ← IndexedState.set("this is a String")
  } yield ()
}
