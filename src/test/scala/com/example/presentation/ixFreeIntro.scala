package com.example
package presentation

object FreeIntro {

  // (Simple) Free monad:
  trait Free[S[_], A] {
    def flatMap[B](f: A ⇒ Free[S, B]): Free[S, B]
  }
}


object IxFreeIntro {

  // Indexed Free monad:
  trait IxFree[S[_, _, _], F, T, A] {
    def flatMap[B, U](f: A ⇒ IxFree[S, T, U, B]):
      IxFree[S, F, U, B]
  }
}
