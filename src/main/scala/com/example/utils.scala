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

import cats.Monad
import cats.data.IndexedStateT

trait FunctionX[F[_, _, _], G[_, _, _]] {
  def apply[A, B, C](x: F[A, B, C]): G[A, B, C]
}

trait IxMonad[S[_, _, _]] {
  def flatMap[F, T, U, A, B](fa: S[F, T, A])(f: A => S[T, U, B]): S[F, U, B]
  def pure[F, A](a: A): S[F, F, A]
  def map[F, T, A, B](fa: S[F, T, A])(f: A => B): S[F, T, B] =
    flatMap(fa)(a => pure(f(a)))
  def tailRecM[F, A, B](a: A)(f: A => S[F, F, Either[A, B]]): S[F, F, B]
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
      override def tailRecM[S1, A, B](a: A)(f: A => IndexedStateT[F, S1, S1, Either[A, B]]): IndexedStateT[F, S1, S1, B] = {
        Monad[IndexedStateT[F, S1, S1, ?]].tailRecM(a)(f)
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
      override def tailRecM[S1, A, B](a: A)(f: A => IndexedStateT[F, R[S1], R[S1], Either[A, B]]): IndexedStateT[F, R[S1], R[S1], B] = {
        Monad[IndexedStateT[F, R[S1], R[S1], ?]].tailRecM(a)(f)
      }
    }
  }

  def monadFromIxMonad[S[_, _, _], F](implicit S: IxMonad[S]): Monad[S[F, F, ?]] = new Monad[S[F, F, ?]] {
    override def flatMap[A, B](sa: S[F, F, A])(f: A => S[F, F, B]): S[F, F, B] =
      S.flatMap(sa)(f)
    override def pure[A](a: A): S[F, F, A] =
      S.pure(a)
    override def tailRecM[A, B](a: A)(f: A => S[F, F, Either[A, B]]): S[F, F, B] = {
      // FIXME: is this stack safe?
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
    override def tailRecM[S1, A, B](a: A)(f: A => S[Either[A, B]]): S[B] =
      S.tailRecM(a)(f)
  }
}
