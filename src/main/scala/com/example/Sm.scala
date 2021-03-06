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
import cats.implicits._

import cats.effect.{ Sync, ExitCase }
import fs2.async.Ref

final class Sm[S[_, _, _], F, T, A] private (
  private val repr: IxFree[S, F, T, A]
) {

  import Sm.{ Execute, ExecuteBracket }

  def flatMap[B, U](f: A => Sm[S, T, U, B]): Sm[S, F, U, B] =
    new Sm(repr.flatMap(a => f(a).repr))

  def map[B](f: A => B): Sm[S, F, T, B] =
    new Sm(repr.map(f))

  // TODO: infer `R`
  def run[M[_] : Monad, R[_]](
    implicit
    exec: Execute.Aux[S, M, R, F, T]
  ): M[A] = for {
    resource <- exec.init
    rr <- execOnly[M, R](resource)
    (_, result) = rr
  } yield result

  def execOnly[M[_] : Monad, R[_]](resource: R[F])(
    implicit
    exec: Execute.Aux[S, M, R, F, T]
  ): M[(R[T], A)] = {
    val fx = new FunctionX[S, Sm.ResRepr[M, exec.Res]#λ] {
      def apply[G, U, X](sa: S[G, U, X]): IndexedStateT[M, exec.Res[G], exec.Res[U], X] = {
        IndexedStateT { res: exec.Res[G] =>
          exec.exec(res)(sa)
        }
      }
    }
    val st = repr.foldMap[Sm.ResRepr[M, exec.Res]#λ](fx)
    st.run(resource)
  }

  def runBracketed[M[_], R[_]](
    implicit
    M: Sync[M],
    exec: ExecuteBracket.Aux[S, M, R, F, T]
  ): M[A] = {
    val acq = for {
      ref <- Ref[M, R[T]](default[R[T]])
      res <- exec.init
    } yield (res, ref)
    M.bracketCase(acq) { rr =>
      val (res, ref) = rr
      execOnly[M, R](res).flatTap { ra =>
        val (finRes, _) = ra
        ref.modify { _ => finRes }
      }.map(_._2)
    } { (rr, ec) =>
      val (initRes, ref) = rr
      ec match {
        case ExitCase.Completed =>
          ref.get.map {
            case null => Left(initRes)
            case ok => Right(ok)
          }.flatMap { either =>
            exec.fin(either)
          }
        case ExitCase.Canceled(_) =>
          exec.fin(Left(initRes))
        case ExitCase.Error(_) =>
          exec.fin(Left(initRes))
      }
    }
  }
}

object Sm {

  type ResRepr[M[_], Res[_]] = {
    type λ[f, t, x] = IndexedStateT[M, Res[f], Res[t], x]
  }

  trait Execute[S[_, _, _]] {
    type M[a]
    type Res[st]
    type InitSt
    type FinSt
    def init: M[Res[InitSt]]
    def exec[F, T, A](res: Res[F])(sa: S[F, T, A]): M[(Res[T], A)]
  }

  object Execute {
    type Aux[S[_, _, _], M0[_], R[_], F, T] = Execute[S] {
      type M[a] = M0[a]
      type InitSt = F
      type FinSt = T
      type Res[st] = R[st]
    }
  }

  trait ExecuteBracket[S[_, _, _]] extends Execute[S] {
    def fin(res: Either[Res[InitSt], Res[FinSt]]): M[Unit]
  }

  object ExecuteBracket {
    type Aux[S[_, _, _], M0[_], R[_], F, T] = ExecuteBracket[S] {
      type M[a] = M0[a]
      type InitSt = F
      type FinSt = T
      type Res[st] = R[st]
    }
  }

  def pure[S[_, _, _], F, A](a: A): Sm[S, F, F, A] =
    new Sm(IxFree.pure(a))

  def liftF[S[_, _, _], F, T, A](sa: S[F, T, A]): Sm[S, F, T, A] =
    new Sm(IxFree.liftF(sa))

  implicit def smIxMonad[S[_, _, _]]: IxMonad[Sm[S, ?, ?, ?]] = new IxMonad[Sm[S, ?, ?, ?]] {

    def flatMap[F, T, U, A, B](fa: Sm[S, F, T, A])(f: A => Sm[S, T, U, B]): Sm[S, F, U, B] =
      fa.flatMap(f)

    def pure[F, A](a: A): Sm[S, F, F, A] =
      Sm.pure(a)

    // This should be okay, since `flatMap` is stack-safe
    def tailRecM[Z[_, _, _], F, T, A, B](a: Z[F, T, A])(f: FunctionTr[Z, Sm[S, ?, ?, ?], A, B])(implicit Z: IxMonad[Z]): Sm[S, F, T, B] =
      this.defaultTailRecM(a)(f)
  }

  implicit def smMonad[S[_, _, _], F]: Monad[Sm[S, F, F, ?]] =
    IxMonad.monadFromIxMonad
}
