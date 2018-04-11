/*
 * Copyright 2018 Daniel Urban and contributors listed in AUTHORS
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

final object HTreeOps {

  sealed trait Filter[T <: HTree, R[_], L <: St.Label[R]] {
    type Out <: HTree
    type Rest <: HTree
  }

  object Filter extends Filter1 {

    type Aux[T <: HTree, R[_], L <: St.Label[R], O <: HTree, RST <: HTree] = Filter[T, R, L] {
      type Out = O
      type Rest = RST
    }

    def apply[T <: HTree, R[_], L <: St.Label[R]](implicit ev: Filter[T, R, L]): Filter.Aux[T, R, L, ev.Out, ev.Rest] =
      ev

    private[HTreeOps] def mk[T <: HTree, R[_], L <: St.Label[R], O <: HTree, RST <: HTree]: Filter.Aux[T, R, L, O, RST] = {
      new Filter[T, R, L] {
        final type Out = O
        final type Rest = RST
      }
    }

    implicit def matNil[R[_], L <: St.Label[R]]: Filter.Aux[HNil, R, L, HNil, HNil] =
      mk
  }

  private[HTreeOps] sealed abstract class Filter1 extends Filter2 { this: Filter.type =>

    implicit def matConsFoundMove[T <: HTree, R[_], S, L <: St.Label[R]](
      implicit tl: Filter[T, R, L]
    ): Filter.Aux[St.Move[R, S, L] :: T, R, L, St.Move[R, S, L] :: tl.Out, tl.Rest] = mk

    implicit def matConsFoundUse[T <: HTree, R[_], S, L <: St.Label[R]](
      implicit tl: Filter[T, R, L]
    ): Filter.Aux[St.InState[R, S, L] :: T, R, L, St.InState[R, S, L] :: tl.Out, tl.Rest] = mk

    implicit def matFork[LHS <: HTree, RHS <: HTree, R[_], L <: St.Label[R]](
      implicit
      lhs: Filter[LHS, R, L],
      rhs: Filter[RHS, R, L]
    ): Filter.Aux[LHS :+: RHS, R, L, lhs.Out :+: rhs.Out, lhs.Rest :+: rhs.Rest] = mk
  }

  private[HTreeOps] sealed abstract class Filter2 { this: Filter.type =>
    implicit def matConsSkip[T <: HTree, R[_], L <: St.Label[R], H](
      implicit tl: Filter[T, R, L]
    ): Filter.Aux[H :: T, R, L, tl.Out, H :: tl.Rest] = mk
  }
}
