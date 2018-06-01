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

  def run[B >: A]: Machine[B, Out[HNil]] =
    new Machine[B, Out[HNil]](this) {}

  protected def unwrap: St.Aux[Res, self.Out]
}

object St {

  type Aux[A, O[_ <: HTree] <: HTree] = St[A] {
    // Note: when using `... = O[In]`, some
    // long for-comprehensions with multiple
    // labels fail (scalac seems to get confused
    // with the various `Lb`s). Using `<:` seems
    // to fix this.
    type Out[In <: HTree] <: O[In]
  }

  type Identity = {
    type λ[i <: HTree] = i
  }

  type Prepend[x] = {
    type λ[i <: HTree] = x :: i
  }

  type Prepend2[x, y] = {
    type λ[i <: HTree] = x :: y :: i
  }

  sealed abstract class Label {
    type Lb <: Label
    type Res[_]
    def delete: St.Aux[Unit, Prepend[Move[Destroyed, this.Lb]]#λ]
    def read[S]: St.Aux[this.Res[S], Prepend[InState[S, this.Lb]]#λ]
    def write[T](r: Res[T]): St.Aux[Unit, Prepend[Move[T, this.Lb]]#λ]
  }

  sealed trait NotCreated
  sealed trait Destroyed

  sealed trait HtEntry[L <: Label]

  sealed trait Move[S, L <: Label] extends HtEntry[L] {
    //type Lb = L
  }

  sealed trait InState[S, L <: Label] extends HtEntry[L] {
    //type Lb = L
  }

  type Create[S, L <: Label] = Move[S, L]

  type Delete[L <: Label] = Move[Destroyed, L]

  object Api {
    trait Initial[R[_], S]
    trait Transition[R[_], F, T]
    type Final[R[_], S] = Transition[R, S, Destroyed]
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

  private[St] final case class Mk[S, L <: Label with Singleton](label: L) extends StImpl[L] {
    final type Out[In <: HTree] = Prepend[Create[S, L]]#λ[In]
  }

  private[St] final case class Del[L <: Label with Singleton](label: L) extends StImpl[Unit] {
    final type Out[In <: HTree] = Prepend[Delete[L]]#λ[In]
  }

  private[St] final case class Rd[S, L <: Label with Singleton](label: L) extends St[L#Res[S]] { self =>

    final type Res = L#Res[S]

    final type Out[In <: HTree] = Prepend[InState[S, L]]#λ[In]

    protected final override def unwrap: St.Aux[Res, self.Out] =
      self
  }

  // TODO: rename to `Wr`
  private[St] final case class Mv[T, L <: Label with Singleton](label: L, value: L#Res[T]) extends StImpl[Unit] {
    final type Out[In <: HTree] = Prepend[Move[T, L]]#λ[In]
  }

  def pure[A](a: A): St.Aux[A, Identity#λ] =
    Pure(a)

  def create[R[_], S](
    implicit @evidence ev: Api.Initial[R, S]
  ): St.Aux[L, Prepend[Create[S, L]]#λ] forSome { type L <: Label with Singleton { type Lb = L; type Res[x] = R[x] } } = {
    val l = new Label {
      type Lb = this.type
      type Res[x] = R[x]
      final override def delete: St.Aux[Unit, Prepend[Delete[this.type]]#λ] = Del[this.type](this)
      final override def read[S1] = Rd[S1, this.type](this)
      final override def write[T1](r: R[T1]) = Mv[T1, this.type](this, r)
    }
    Mk[S, l.type](l)
  }

  // Execution:

  sealed abstract class Machine[A, H <: HTree](st: St[A]) {

    type Ops = H

    type Res = A

    // TODO: parameterized effect type
    def run(implicit @evidence ev: HTreeOps.ConsistentTree[st.Out[HNil]]): IO[A] =
      interpreter(st)

    def unsafeRun: IO[A] =
      interpreter(st)

    def debug(implicit dbg: HTree.Debug[Ops]): String =
      dbg.debug
  }

  // TODO: stack safety, parameterized effect type, force constraints, ...
  private def interpreter: St ~> IO = new ~>[St, IO] {
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
