/*
 * Copyright 2016-2018 Daniel Urban and contributors listed in AUTHORS
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

import scala.language.existentials

import cats.Monad
import cats.implicits._
import cats.data.IndexedStateT

final class evidence extends deprecated("this parameter is only used as evidence", "")

trait FunctionX[F[_, _, _], G[_, _, _]] {
  def apply[A, B, C](x: F[A, B, C]): G[A, B, C]
}

trait FunctionTr[F[_, _, _], G[_, _, _], X, Y] {
  def apply[A, B](x: F[A, B, X]): G[A, b, FunctionTr.Xor[F, b, B, X, Y]] forSome { type b }
}

object FunctionTr {

  sealed abstract class Xor[F[_, _, _], A, B, X, Y] {
    final type From = A
  }

  final case class Rec[F[_, _, _], A, B, X, Y](a: F[A, B, X]) extends Xor[F, A, B, X, Y]
  final case class Done[F[_, _, _], A, X, Y](b: Y) extends Xor[F, A, A, X, Y]

  def rec[F[_, _, _], A, B, X, Y](a: F[A, B, X]): Xor[F, A, B, X, Y] =
    Rec(a)

  def done[F[_, _, _], A, X, Y](b: Y): Xor[F, A, A, X, Y] =
    Done(b)
}

trait IxMonad[S[_, _, _]] {
  def flatMap[F, T, U, A, B](fa: S[F, T, A])(f: A => S[T, U, B]): S[F, U, B]
  def pure[F, A](a: A): S[F, F, A]
  def map[F, T, A, B](fa: S[F, T, A])(f: A => B): S[F, T, B] =
    flatMap(fa)(a => pure(f(a)))
  def tailRecM[Z[_, _, _], F, T, A, B](a: Z[F, T, A])(f: FunctionTr[Z, S, A, B])(implicit Z: IxMonad[Z]): S[F, T, B]

  /** Assumes a stack-safe flatMap */
  protected final def defaultTailRecM[Z[_, _, _], F, T, A, B](a: Z[F, T, A])(f: FunctionTr[Z, S, A, B])(implicit Z: IxMonad[Z]): S[F, T, B] = {
    flatMap(f[F, T](a)) { x =>
      x match {
        case r: FunctionTr.Rec[Z, f, T, A, B] =>
          defaultTailRecM(r.a)(f)
        case d: FunctionTr.Done[Z, f, A, B] =>
          pure[T, B](d.b)
      }
    }
  }
}

object IxMonad {

  type Fake[S[_]] = {
    type λ[f, t, a] = S[a]
  }

  implicit def ixMonadForIndexedStateT[F[_]](implicit F: Monad[F]): IxMonad[IndexedStateT[F, ?, ?, ?]] = {
    new IxMonad[IndexedStateT[F, ?, ?, ?]] {
      override def flatMap[S1, S2, S3, A, B](fa: IndexedStateT[F, S1, S2, A])(
        f: A => IndexedStateT[F, S2, S3, B]
      ): IndexedStateT[F, S1, S3, B] = fa.flatMap(f)
      override def pure[S1, A](a: A): IndexedStateT[F, S1, S1, A] = IndexedStateT { s: S1 =>
        F.map(F.pure(a))(a => (s, a))
      }
      override def tailRecM[Z[_, _, _], S1, S2, A, B](a: Z[S1, S2, A])(f: FunctionTr[Z, IndexedStateT[F, ?, ?, ?], A, B])(implicit Z: IxMonad[Z]): IndexedStateT[F, S1, S2, B] = {
        defaultTailRecM(a)(f)
      }
    }
  }

  implicit def ixMonadForResIndexedStateT[F[_], R[_]](implicit F: Monad[F]): IxMonad[Sm.ResRepr[F, R]#λ] = {
    new IxMonad[Sm.ResRepr[F, R]#λ] {
      override def flatMap[S1, S2, S3, A, B](fa: IndexedStateT[F, R[S1], R[S2], A])(
        f: A => IndexedStateT[F, R[S2], R[S3], B]
      ): IndexedStateT[F, R[S1], R[S3], B] = fa.flatMap(f)
      override def pure[S1, A](a: A): IndexedStateT[F, R[S1], R[S1], A] = IndexedStateT { s: R[S1] =>
        F.map(F.pure(a))(a => (s, a))
      }
      override def tailRecM[Z[_, _, _], S1, S2, A, B](a: Z[S1, S2, A])(f: FunctionTr[Z, Sm.ResRepr[F, R]#λ, A, B])(implicit Z: IxMonad[Z]): IndexedStateT[F, R[S1], R[S2], B] = {
        defaultTailRecM(a)(f)
      }
    }
  }

  def monadFromIxMonad[S[_, _, _], F](implicit S: IxMonad[S]): Monad[S[F, F, ?]] = new Monad[S[F, F, ?]] {
    override def flatMap[A, B](sa: S[F, F, A])(f: A => S[F, F, B]): S[F, F, B] =
      S.flatMap(sa)(f)
    override def pure[A](a: A): S[F, F, A] =
      S.pure(a)
    override def tailRecM[A, B](a: A)(f: A => S[F, F, Either[A, B]]): S[F, F, B] = {
      // TODO: This is only stack-safe if S has
      // TODO: a stack-safe flatMap. We should
      // TODO: try to use IxMonad#tailRecM instead.
      S.flatMap(f(a)) {
        case Left(a) => tailRecM(a)(f)
        case Right(b) => S.pure(b)
      }
    }
  }

  def ixMonadFromMonad[S[_]](implicit S: Monad[S]): IxMonad[Fake[S]#λ] = new IxMonad[Fake[S]#λ] {

    override def flatMap[F, T, U, A, B](fa: S[A])(f: A => S[B]): S[B] =
      S.flatMap(fa)(f)

    override def pure[F, A](a: A): S[A] =
      S.pure(a)

    override def tailRecM[Z[_, _, _], F, T, A, B](a: Z[F, T, A])(f: FunctionTr[Z, Fake[S]#λ, A, B])(implicit Z: IxMonad[Z]): S[B] = {

      sealed abstract class Result {
        type F
        val value: Z[F, T, A]
      }

      final object Result {
        def apply[F0](v: Z[F0, T, A]): Result = new Result {
          type F = F0
          val value = v
        }
      }

      S.tailRecM[Result, B](Result(a)) { res =>
        f(res.value).map {
          case FunctionTr.Done(b) => Right(b)
          case FunctionTr.Rec(a) => Left(Result(a))
        }
      }
    }
  }
}
