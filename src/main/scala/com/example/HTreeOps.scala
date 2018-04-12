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

  /** Filters the steps of one label from the tree */
  sealed abstract class Filter[T <: HTree, R[_], L <: St.Label[R]] {
    type Out <: HTree
    type Rest <: HTree
  }

  final object Filter extends Filter1 {

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

  /** Witnesses that the resource is used consistently */
  sealed abstract class ConsistentResource[T <: HTree, R[_], L <: St.Label[R] { type Lb = L }]

  final object ConsistentResource {

    def apply[T <: HTree, R[_], L <: St.Label[R] { type Lb = L }](implicit ev: ConsistentResource[T, R, L]): ConsistentResource[T, R, L] =
      ev

    implicit def mat[T <: HTree, O <: HTree, R[_], L <: St.Label[R] { type Lb = L }](
      implicit
      @evidence dae: DestroyedAtEnd.Aux[T, R, L, O],
      @evidence tcs: TakesCorrectSteps[O, R, L]
    ): ConsistentResource[T, R, L] = new ConsistentResource[T, R, L] {}
  }

  /** Witnesses that the resource is destroyed after usage */
  sealed abstract class DestroyedAtEnd[T <: HTree, R[_], L <: St.Label[R]] {
    type Out <: HTree
  }

  final object DestroyedAtEnd extends DestroyedAtEnd1 {

    type Aux[T <: HTree, R[_], L <: St.Label[R], O <: HTree] = DestroyedAtEnd[T, R, L] {
      type Out = O
    }

    def apply[T <: HTree, R[_], L <: St.Label[R]](implicit ev: DestroyedAtEnd[T, R, L]): DestroyedAtEnd.Aux[T, R, L, ev.Out] =
      ev

    private[HTreeOps] def mk[T <: HTree, R[_], L <: St.Label[R], O <: HTree]: DestroyedAtEnd.Aux[T, R, L, O] =
      new DestroyedAtEnd[T, R, L] { type Out = O }

    implicit def matSingle[T <: HTree, R[_], L <: St.Label[R]]: DestroyedAtEnd.Aux[St.Delete[R, L] :: T, R, L, T] =
      mk
  }

  private[HTreeOps] sealed abstract class DestroyedAtEnd1 { this: DestroyedAtEnd.type =>
    implicit def matFork[LHS <: HTree, RHS <: HTree, R[_], L <: St.Label[R]]: DestroyedAtEnd.Aux[(St.Delete[R, L] :: LHS) :+: (St.Delete[R, L] :: RHS), R, L, LHS :+: RHS] =
      mk
  }

  /**
   * Witnesses that during usage, there are only correct steps/transitions.
   *
   * Correct steps are:<br>
   * - move then move (between allowed states)<br>
   * - move then read (the correct state)<br>
   * - read then move (to allowed state)<br>
   * - read then read (the same state)<br>
   * - move to init state (at the beginning)<br>
   * - forks<br>
   * - having exactly 0 steps is also correct
   */
  sealed abstract class TakesCorrectSteps[T <: HTree, R[_], L <: St.Label[R]]

  final object TakesCorrectSteps extends TakesCorrectSteps1 {

    def apply[T <: HTree, R[_], L <: St.Label[R]](implicit ev: TakesCorrectSteps[T, R, L]): TakesCorrectSteps[T, R, L] =
      ev

    private[HTreeOps] def mk[T <: HTree, R[_], L <: St.Label[R]]: TakesCorrectSteps[T, R, L] =
      new TakesCorrectSteps[T, R, L] {}

    implicit def moveMove[T <: HTree, S1, S2, R[_], L <: St.Label[R]](
      implicit
      @evidence transition: St.Api.Transition[R, S1, S2],
      @evidence tail: TakesCorrectSteps[St.Move[R, S1, L] :: T, R, L]
    ): TakesCorrectSteps[St.Move[R, S2, L] :: St.Move[R, S1, L] :: T, R, L] = mk
  }

  private[HTreeOps] sealed abstract class TakesCorrectSteps1 extends TakesCorrectSteps2 { this: TakesCorrectSteps.type =>
    implicit def moveRead[T <: HTree, S, R[_], L <: St.Label[R]](
      implicit @evidence tail: TakesCorrectSteps[St.Move[R, S, L] :: T, R, L]
    ): TakesCorrectSteps[St.InState[R, S, L] :: St.Move[R, S, L] :: T, R, L] = mk
  }

  private[HTreeOps] sealed abstract class TakesCorrectSteps2 extends TakesCorrectSteps3 { this: TakesCorrectSteps.type =>
    implicit def readMove[T <: HTree, S1, S2, R[_], L <: St.Label[R]](
      implicit
      @evidence transition: St.Api.Transition[R, S1, S2],
      @evidence tail: TakesCorrectSteps[St.InState[R, S1, L] :: T, R, L]
    ): TakesCorrectSteps[St.Move[R, S2, L] :: St.InState[R, S1, L] :: T, R, L] = mk
  }

  private[HTreeOps] sealed abstract class TakesCorrectSteps3 extends TakesCorrectSteps4 { this: TakesCorrectSteps.type =>
    implicit def readRead[T <: HTree, S, R[_], L <: St.Label[R]](
      implicit @evidence tail: TakesCorrectSteps[St.InState[R, S, L] :: T, R, L]
    ): TakesCorrectSteps[St.InState[R, S, L] :: St.InState[R, S, L] :: T, R, L] = mk
  }

  private[HTreeOps] sealed abstract class TakesCorrectSteps4 extends TakesCorrectSteps5 { this: TakesCorrectSteps.type =>
    implicit def moveToInit[T <: HTree, S, R[_], L <: St.Label[R]](
      implicit @evidence init: St.Api.Initial[R, S]
    ): TakesCorrectSteps[St.Move[R, S, L] :: HNil, R, L] = mk
  }

  private[HTreeOps] sealed abstract class TakesCorrectSteps5 extends TakesCorrectSteps6 { this: TakesCorrectSteps.type =>
    implicit def fork[LHS <: HTree, RHS <: HTree, R[_], L <: St.Label[R]](
      implicit
      @evidence lhs: TakesCorrectSteps[LHS, R, L],
      @evidence rhs: TakesCorrectSteps[RHS, R, L]
    ): TakesCorrectSteps[LHS :+: RHS, R, L] = mk
  }

  private[HTreeOps] sealed abstract class TakesCorrectSteps6 { this: TakesCorrectSteps.type =>
    implicit def end[R[_], L <: St.Label[R]]: TakesCorrectSteps[HNil, R, L] =
      mk
  }
}
