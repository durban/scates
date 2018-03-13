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

import scala.annotation.tailrec

sealed abstract class IxFree[S[_, _, _], F, T, A] extends Product with Serializable {

  import IxFree._

  def flatMap[B, U](f: A => IxFree[S, T, U, B]): IxFree[S, F, U, B] =
    FlatMapped(this, f)

  def map[B](f: A => B): IxFree[S, F, T, B] =
    flatMap(a => Pure(f(a)))

  @tailrec
  final def step: IxFree[S, F, T, A] = {
    val r = this match {
      case FlatMapped(FlatMapped(c, f), g) =>
        Left(c.flatMap(cc => f(cc).flatMap(g)))
      case fm: FlatMapped[S, F, u, T, b, A] =>
        fm.s match {
          case p: Pure[S, f, A] =>
            Left(fm.f(p.a))
          case _ =>
            Right(fm)
        }
      case x =>
        Right(x)
    }

    r match {
      case Left(rec) =>
        rec.step
      case Right(done) =>
        done
    }
  }

  final def foldMap[M[_, _, _]](f: FunctionX[S[?, ?, ?], M[?, ?, ?]])(implicit M: IxMonad[M]): M[F, T, A] = {
    M.tailRecM(this)(new FunctionTr[IxFree[S, ?, ?, ?], M, A, A] {
      override def apply[G, U](curr: IxFree[S, G, U, A]): M[G, X, FunctionTr.Xor[IxFree[S, ?, ?, ?], X, U, A, A]] forSome { type X } = {

        /**
         * Wraps the result in the `match` below
         * (this is a trick, because scalac gets
         * confused if we use `forSome` existentials).
         */
        sealed abstract class Result {
          type X
          val value: M[G, X, FunctionTr.Xor[IxFree[S, ?, ?, ?], X, U, A, A]]
        }

        /** Factory for `Result` */
        final object Result {
          def apply[X0](v: M[G, X0, FunctionTr.Xor[IxFree[S, ?, ?, ?], X0, U, A, A]]): Result = new Result {
            type X = X0
            val value = v
          }
        }

        val result: Result = curr.step match {
          case p: Pure[S, G, A] =>
            Result(M.pure[G, FunctionTr.Xor[IxFree[S, ?, ?, ?], G, G, A, A]](FunctionTr.done(p.a)))
          case s: Suspend[S, G, U, A] =>
            Result(M.map(f(s.s))(a => FunctionTr.done[IxFree[S, ?, ?, ?], U, A, A](a)))
          case fm: FlatMapped[S, G, u, U, b, A] =>
            val xxx = fm.s.foldMap(f)
            Result(M.map(xxx)(cc => FunctionTr.rec[IxFree[S, ?, ?, ?], u, U, A, A](fm.f(cc))))
        }

        result.value
      }
    })
  }
}

object IxFree {

  def pure[S[_, _, _], F, A](a: A): IxFree[S, F, F, A] =
    Pure(a)

  def liftF[S[_, _, _], F, T, A](sa: S[F, T, A]): IxFree[S, F, T, A] =
    Suspend(sa)

  final case class Pure[S[_, _, _], F, A](a: A)
    extends IxFree[S, F, F, A]

  final case class Suspend[S[_, _, _], F, T, A](s: S[F, T, A])
    extends IxFree[S, F, T, A]

  final case class FlatMapped[S[_, _, _], F, T, U, A, B](
    s: IxFree[S, F, T, A], f: A => IxFree[S, T, U, B]
  ) extends IxFree[S, F, U, B]

  implicit def ixMonadInstance[S[_, _, _]]: IxMonad[IxFree[S, ?, ?, ?]] = new IxMonad[IxFree[S, ?, ?, ?]] {
    def flatMap[F, T, U, A, B](fa: IxFree[S, F, T, A])(f: A => IxFree[S, T, U, B]): IxFree[S, F, U, B] =
      fa flatMap f
    def pure[F, A](a: A): IxFree[S, F, F, A] =
      pure(a)
    def tailRecM[Z[_, _, _], F, T, A, B](a: Z[F, T, A])(f: FunctionTr[Z, IxFree[S, ?, ?, ?], A, B])(implicit Z: IxMonad[Z]): IxFree[S, F, T, B] =
      defaultTailRecM(a)(f)
  }
}
