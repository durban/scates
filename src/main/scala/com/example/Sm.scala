package com.example

import cats.Monad

final class Sm[S[_, _, _], F, T, A] private (private val repr: IxFree[S, F, T, A]) {

  import Sm.{ Initial, Final, Interpreter }

  def flatMap[B, U](f: A => Sm[S, T, U, B]): Sm[S, F, U, B] =
    new Sm(repr.flatMap(a => f(a).repr))

  def map[B](f: A => B): Sm[S, F, T, B] =
    new Sm(repr.map(f))

  def run[M[_] : Monad](i: Interpreter[S, M])(implicit F: Initial[S, F], T: Final[S, T]): M[A] = {
    val fx = new FunctionX[S, IxMonad.Fake[M]#λ] {
      def apply[G, U, X](sa: S[G, U, X]): M[X] =
        i.apply(sa)
    }
    repr.foldMap[IxMonad.Fake[M]#λ](fx)(IxMonad.ixMonadFromMonad[M])
  }
}

object Sm {

  trait Initial[S[_, _, _], A]

  trait Final[S[_, _, _], A]

  trait Interpreter[S[_, _, _], M[_]] {
    def apply[F, T, A](sa: S[F, T, A]): M[A]
  }

  def pure[S[_, _, _], F, A](a: A)(implicit F: Initial[S, F]): Sm[S, F, F, A] =
    new Sm(IxFree.pure(a))

  def liftF[S[_, _, _], F, T, A](sa: S[F, T, A]): Sm[S, F, T, A] =
    new Sm(IxFree.liftF(sa))
}
