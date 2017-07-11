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
    // XXX: use tailRecM to make it stack-safe
    def go[G, U, X](curr: IxFree[S, G, U, X]): M[G, U, X] = curr.step match {
      case p: Pure[S, G, X] =>
        M.pure[G, X](p.a)
      case s: Suspend[S, G, U, X] =>
        f(s.s)
      case fm: FlatMapped[S, G, u, U, y, X] =>
        val xxx = fm.s.foldMap(f)
        M.flatMap(xxx)(cc => go(fm.f(cc)))
    }
    go(this)
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
    def tailRecM[F, A, B](a: A)(f: A => IxFree[S, F, F, Either[A, B]]): IxFree[S, F, F, B] = ??? // TODO
  }
}
