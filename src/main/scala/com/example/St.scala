/*
 * Copyright 2017-2018 Daniel Urban and contributors listed in AUTHORS
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

import cats.~>
import cats.effect.IO
import cats.evidence.As

sealed abstract class St[+A] { self =>

  import St._

  type Res <: A

  type Out[In <: HTree] <: HTree

  def flatMap[B, S <: St[_]](f: A => S)(implicit ev: S#Res <:< B): St.Aux[B, ({ type λ[i <: HTree] = S#Out[Out[i]] })#λ] =
    FlatMap[A, B, Out, S](self, f)

  def map[B](f: A => B): St.Aux[B, Out] =
    flatMap[B, St.Aux[B, Identity#λ]](a => St.pure[B](f(a)))

  protected def unwrap: St.Aux[Res, self.Out]
}

object St {

  type Aux[A, O[_ <: HTree] <: HTree] = St[A] {
    type Out[In <: HTree] = O[In] // maybe `<:`?
  }

  type Identity = {
    type λ[i <: HTree] = i
  }

  type Prepend[x] = {
    type λ[i <: HTree] = x :: i
  }

  sealed abstract class Label[R[_]] {
    type Lb <: Label[R]
    def delete: St.Aux[Unit, Prepend[Delete[R, Lb]]#λ]
    def read[S]: St.Aux[R[S], Prepend[InState[R, S, Lb]]#λ]
    def write[T](r: R[T]): St.Aux[Unit, Prepend[Move[R, T, Lb]]#λ]
  }

  sealed trait NotCreated
  sealed trait Destroyed

  sealed trait Move[R[_], S, L <: Label[R]] {
    type Lb = L
  }

  sealed trait InState[R[_], S, L <: Label[R]] {
    type Lb = L
  }

  type Create[R[_], S, L <: Label[R]] = Move[R, S, L]

  type Delete[R[_], L <: Label[R]] = Move[R, Destroyed, L]

  object Api {
    trait Initial[R[_], S]
    trait Final[R[_], S]
    trait Transition[R[_], F, T]
  }

  // TODO: can we reuse Free?

  private[St] sealed abstract class StImpl[A] extends St[A] { self =>

    final type Res = A

    protected final override def unwrap: St.Aux[Res, self.Out] =
      self
  }

  private[St] final case class Pure[A](a: A) extends StImpl[A] {
    final type Out[In <: HTree] = Identity#λ[In]
  }

  private[St] final case class FlatMap[A, B, O1[_ <: HTree] <: HTree, S <: St[_]](st: St.Aux[A, O1], f: A => S)(implicit ev: S#Res <:< B) extends StImpl[B] {

    final type Out[In <: HTree] = S#Out[O1[In]]

    private[this] val evas: As[St[S#Res], St[B]] =
      As.co[St, S#Res, B](As.fromPredef(ev))

    def func: A => St[B] = { a: A =>
      evas.coerce(f(a).unwrap)
    }
  }

  private[St] final case class Mk[R[_], S, L <: Label[R] { type Lb = L }](label: L) extends StImpl[L] {
    final type Out[In <: HTree] = Prepend[Create[R, S, L]]#λ[In]
  }

  private[St] final case class Del[R[_], L <: Label[R] with Singleton](label: L) extends StImpl[Unit] {
    final type Out[In <: HTree] = Prepend[Delete[R, L#Lb]]#λ[In]
  }

  private[St] final case class Rd[R[_], S, L <: Label[R] with Singleton](label: L) extends St[R[S]] { self =>

    final type Res = R[S]

    final type Out[In <: HTree] = Prepend[InState[R, S, L#Lb]]#λ[In]

    protected final override def unwrap: St.Aux[Res, self.Out] =
      self
  }

  // TODO: rename to `Wr`
  private[St] final case class Mv[R[_], T, L <: Label[R] { type Lb = L }](label: L, value: R[T]) extends StImpl[Unit] {
    final type Out[In <: HTree] = Prepend[Move[R, T, L]]#λ[In]
  }

  def pure[A](a: A): St.Aux[A, Identity#λ] =
    Pure(a)

  // TODO: infer `S`
  def create[R[_], S](implicit @evidence ev: Api.Initial[R, S]): St.Aux[L, Prepend[Create[R, S, L]]#λ] forSome { type L <: Label[R] { type Lb = L } } = {
    val l = new Label[R] {
      type Lb = this.type
      final override def delete: St.Aux[Unit, Prepend[Delete[R, Lb]]#λ] = Del[R, this.type](this)
      final override def read[S1] = Rd[R, S1, this.type](this)
      final override def write[T1](r: R[T1]) = Mv[R, T1, this.type](this, r)
    }
    Mk[R, S, l.type](l)
  }

  // TODO: constrain to final states
  // FIXME: type inference is bad for this
  // FIXME: do we need this? The one on `Label` should be enough ...
  def delete[R[_], L <: Label[R] { type Lb = L }](label: L): St.Aux[Unit, Prepend[Delete[R, label.Lb]]#λ] =
    Del[R, label.type](label)

  // Type classes for consistency checking:

  sealed abstract class WellFormed[T <: HTree] // TODO

  // Execution:

  // TODO: parameterized effect type
  def run[A](st: St[A])(implicit @evidence ev: WellFormed[st.Out[HNil]]): IO[A] =
    interpreter(st)

  // TODO: stack safety, parameterized effect type, force constraints, ...
  def interpreter: St ~> IO = new ~>[St, IO] {
    def apply[A](st: St[A]): IO[A] = st match {
      case Pure(a) =>
        IO.pure(a)
      case fm: FlatMap[a, b, o1, s] =>
        apply(fm.st).flatMap { a =>
          apply(fm.func(a))
        }
      case mk @ Mk(_) =>
        IO.pure(mk.label)
      case Del(label) =>
        IO {
          // TODO: deleting
          ()
        }
      case Rd(_) =>
        ??? // TODO
      case Mv(_, _) =>
        ??? // TODO
    }
  }
}
