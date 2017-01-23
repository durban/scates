package com.example

import cats.Monad

trait FunctionX[F[_, _, _], G[_, _, _]] {
  def apply[A, B, C](x: F[A, B, C]): G[A, B, C]
}

trait IxMonad[S[_, _, _]] {
  def flatMap[F, T, U, A, B](fa: S[F, T, A])(f: A => S[T, U, B]): S[F, U, B]
  def pure[F, A](a: A): S[F, F, A]
  def map[F, T, A, B](fa: S[F, T, A])(f: A => B): S[F, T, B] =
    flatMap(fa)(a => pure(f(a)))
}

object IxMonad {

  type Fake[S[_]] = {
    type λ[f, t, a] = S[a]
  }

  def monadFromIxMonad[S[_, _, _], F](implicit S: IxMonad[S]): Monad[S[F, F, ?]] = new Monad[S[F, F, ?]] {
    override def flatMap[A, B](sa: S[F, F, A])(f: A => S[F, F, B]): S[F, F, B] =
      S.flatMap(sa)(f)
    override def pure[A](a: A): S[F, F, A] =
      S.pure(a)
    override def tailRecM[A, B](a: A)(f: A => S[F, F, Either[A, B]]): S[F, F, B] =
      ??? // TODO
  }

  def ixMonadFromMonad[S[_]](implicit S: Monad[S]): IxMonad[Fake[S]#λ] = new IxMonad[Fake[S]#λ] {
    override def flatMap[F, T, U, A, B](fa: S[A])(f: A => S[B]): S[B] =
      S.flatMap(fa)(f)
    override def pure[F, A](a: A): S[A] =
      S.pure(a)
  }
}
