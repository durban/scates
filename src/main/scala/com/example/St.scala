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

  sealed trait Label[A] {
    type Lb <: Label[A]
    def delete: St.Aux[Unit, Prepend[Delete[Lb]]#λ]
  }

  sealed trait Create[L <: Label[_]] {
    type Lb = L
  }

  sealed trait Delete[L <: Label[_]] {
    type Lb = L
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

  private[St] final case class Mk[A, L <: Label[A] { type Lb = L }](resource: A, label: L) extends StImpl[L] {
    final type Out[In <: HTree] = Prepend[Create[L]]#λ[In]
  }

  private[St] final case class Del[A, L <: Label[A] with Singleton](label: L) extends StImpl[Unit] {
    final type Out[In <: HTree] = Prepend[Delete[L#Lb]]#λ[In]
  }

  def pure[A](a: A): St.Aux[A, Identity#λ] =
    Pure(a)

  // TODO: specify `resource` in a monad?
  // TODO: constrain to initial states
  def create[A](resource: A): St.Aux[L, Prepend[Create[L]]#λ] forSome { type L <: Label[A] { type Lb = L } } = {
    val l = new Label[A] {
      type Lb = this.type
      final override def delete: St.Aux[Unit, Prepend[Delete[Lb]]#λ] = Del[A, this.type](this)
    }
    Mk[A, l.type](resource, l)
  }

  // TODO: constrain to final states
  // FIXME: type inference is bad for this
  def delete[A, L <: Label[A] { type Lb = L }](label: L): St.Aux[Unit, Prepend[Delete[label.Lb]]#λ] =
    Del[A, label.type](label)

  // TODO: stack safety, parameterized effect type, force constraints, ...
  def interpreter: St ~> IO = new ~>[St, IO] {
    def apply[A](st: St[A]): IO[A] = st match {
      case Pure(a) =>
        IO.pure(a)
      case fm: FlatMap[a, b, o1, s] =>
        apply(fm.st).flatMap { a =>
          apply(fm.func(a))
        }
      case mk @ Mk(_, _) =>
        IO.pure(mk.label)
      case Del(label) =>
        IO {
          // TODO: deleting
          ()
        }
    }
  }
}
