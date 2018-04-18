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
  sealed abstract class Filter[T <: HTree, L <: St.Label] {
    type Out <: HTree
    type Rest <: HTree
  }

  final object Filter extends Filter1 {

    type Aux[T <: HTree, L <: St.Label, O <: HTree, RST <: HTree] = Filter[T, L] {
      type Out = O
      type Rest = RST
    }

    def apply[T <: HTree, L <: St.Label](implicit ev: Filter[T, L]): Filter.Aux[T, L, ev.Out, ev.Rest] =
      ev

    private[HTreeOps] def mk[T <: HTree, R[_], L <: St.Label, O <: HTree, RST <: HTree]: Filter.Aux[T, L, O, RST] = {
      new Filter[T, L] {
        final type Out = O
        final type Rest = RST
      }
    }

    implicit def matNil[L <: St.Label]: Filter.Aux[HNil, L, HNil, HNil] =
      mk
  }

  private[HTreeOps] sealed abstract class Filter1 extends Filter2 { this: Filter.type =>
    implicit def matConsFoundMove[T <: HTree, S, L <: St.Label](
      implicit tl: Filter[T, L]
    ): Filter.Aux[St.Move[S, L] :: T, L, St.Move[S, L] :: tl.Out, tl.Rest] = mk
  }

  private[HTreeOps] sealed abstract class Filter2 extends Filter3 { this: Filter.type =>
    implicit def matConsFoundUse[T <: HTree, S, L <: St.Label](
      implicit tl: Filter[T, L]
    ): Filter.Aux[St.InState[S, L] :: T, L, St.InState[S, L] :: tl.Out, tl.Rest] = mk
  }

  private[HTreeOps] sealed abstract class Filter3 extends Filter4 { this: Filter.type =>
    implicit def matFork[LHS <: HTree, RHS <: HTree, L <: St.Label](
      implicit
      lhs: Filter[LHS, L],
      rhs: Filter[RHS, L]
    ): Filter.Aux[LHS :+: RHS, L, lhs.Out :+: rhs.Out, lhs.Rest :+: rhs.Rest] = mk
  }

  private[HTreeOps] sealed abstract class Filter4 { this: Filter.type =>
    implicit def matConsSkip[T <: HTree, L <: St.Label, H](
      implicit tl: Filter[T, L]
    ): Filter.Aux[H :: T, L, tl.Out, H :: tl.Rest] = mk
  }

  /** Witnesses that all resources in the tree are used consistently */
  sealed abstract class ConsistentTree[T <: HTree]

  final object ConsistentTree extends ConsistentTree1 {

    def apply[T <: HTree](implicit ev: ConsistentTree[T]): ConsistentTree[T] =
      ev

    private[HTreeOps] def mk[T <: HTree]: ConsistentTree[T] =
      new ConsistentTree[T] {}

    implicit def matCons[T <: HTree, L <: St.Label, F <: HTree, G <: HTree](
      implicit
      @evidence head: HeadResource.Aux[T, L],
      @evidence filter: Filter.Aux[T, L, F, G],
      @evidence res1: ConsistentResource[F, L],
      @evidence others: ConsistentTree[G]
    ): ConsistentTree[T] = mk
  }

  private[HTreeOps] sealed abstract class ConsistentTree1 extends ConsistentTree2 { this: ConsistentTree.type =>
    implicit def matFork[LHS <: HTree, RHS <: HTree](
      implicit
      @evidence lhs: ConsistentTree[LHS],
      @evidence rhs: ConsistentTree[RHS]
    ): ConsistentTree[LHS :+: RHS] = mk
  }

  private[HTreeOps] sealed abstract class ConsistentTree2 { this: ConsistentTree.type =>
    implicit def matNil: ConsistentTree[HNil] =
      mk
  }

  /** Extracts the first resource in a HList */
  sealed abstract class HeadResource[T <: HTree] {
    type L <: St.Label
  }

  final object HeadResource {

    type Aux[T <: HTree, L0 <: St.Label] = HeadResource[T] {
      type L = L0
    }

    def apply[T <: HTree](implicit ev: HeadResource[T]): HeadResource.Aux[T, ev.L] =
      ev

    private[HTreeOps] def mk[T <: HTree, L0 <: St.Label]: HeadResource.Aux[T, L0] =
      new HeadResource[T] { final type L = L0 }

    implicit def matMove[T <: HTree, S, L <: St.Label]: HeadResource.Aux[St.Move[S, L] :: T, L] =
      mk

    implicit def matRead[T <: HTree, S, L <: St.Label]: HeadResource.Aux[St.InState[S, L] :: T, L] =
      mk
  }

  /** Witnesses that the resource is used consistently */
  sealed abstract class ConsistentResource[T <: HTree, L <: St.Label]

  final object ConsistentResource {

    def apply[T <: HTree, L <: St.Label](implicit ev: ConsistentResource[T, L]): ConsistentResource[T, L] =
      ev

    implicit def mat[T <: HTree, L <: St.Label](
      implicit
      @evidence dae: DestroyedAtEnd[T, L],
      @evidence tcs: TakesCorrectSteps[T, L]
    ): ConsistentResource[T, L] = new ConsistentResource[T, L] {}
  }

  /** Witnesses that the resource is destroyed after usage */
  sealed abstract class DestroyedAtEnd[T <: HTree, L <: St.Label]

  final object DestroyedAtEnd extends DestroyedAtEnd1 {

    def apply[T <: HTree, L <: St.Label](implicit ev: DestroyedAtEnd[T, L]): DestroyedAtEnd[T, L] =
      ev

    private[HTreeOps] def mk[T <: HTree, L <: St.Label]: DestroyedAtEnd[T, L] =
      new DestroyedAtEnd[T, L] {}

    implicit def matSingle[T <: HTree, L <: St.Label]: DestroyedAtEnd[St.Delete[L] :: T, L] =
      mk
  }

  private[HTreeOps] sealed abstract class DestroyedAtEnd1 { this: DestroyedAtEnd.type =>
    implicit def matFork[LHS <: HTree, RHS <: HTree, L <: St.Label]: DestroyedAtEnd[(St.Delete[L] :: LHS) :+: (St.Delete[L] :: RHS), L] =
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
  sealed abstract class TakesCorrectSteps[T <: HTree, L <: St.Label]

  final object TakesCorrectSteps extends TakesCorrectSteps1 {

    def apply[T <: HTree, L <: St.Label](implicit ev: TakesCorrectSteps[T, L]): TakesCorrectSteps[T, L] =
      ev

    private[HTreeOps] def mk[T <: HTree, L <: St.Label]: TakesCorrectSteps[T, L] =
      new TakesCorrectSteps[T, L] {}

    implicit def moveMove[T <: HTree, S1, S2, L <: St.Label](
      implicit
      @evidence transition: St.Api.Transition[L#Res, S1, S2],
      @evidence tail: TakesCorrectSteps[St.Move[S1, L] :: T, L]
    ): TakesCorrectSteps[St.Move[S2, L] :: St.Move[S1, L] :: T, L] = mk
  }

  private[HTreeOps] sealed abstract class TakesCorrectSteps1 extends TakesCorrectSteps2 { this: TakesCorrectSteps.type =>
    implicit def moveRead[T <: HTree, S, L <: St.Label](
      implicit @evidence tail: TakesCorrectSteps[St.Move[S, L] :: T, L]
    ): TakesCorrectSteps[St.InState[S, L] :: St.Move[S, L] :: T, L] = mk
  }

  private[HTreeOps] sealed abstract class TakesCorrectSteps2 extends TakesCorrectSteps3 { this: TakesCorrectSteps.type =>
    implicit def readMove[T <: HTree, S1, S2, L <: St.Label](
      implicit
      @evidence transition: St.Api.Transition[L#Res, S1, S2],
      @evidence tail: TakesCorrectSteps[St.InState[S1, L] :: T, L]
    ): TakesCorrectSteps[St.Move[S2, L] :: St.InState[S1, L] :: T, L] = mk
  }

  private[HTreeOps] sealed abstract class TakesCorrectSteps3 extends TakesCorrectSteps4 { this: TakesCorrectSteps.type =>
    implicit def readRead[T <: HTree, S, L <: St.Label](
      implicit @evidence tail: TakesCorrectSteps[St.InState[S, L] :: T, L]
    ): TakesCorrectSteps[St.InState[S, L] :: St.InState[S, L] :: T, L] = mk
  }

  private[HTreeOps] sealed abstract class TakesCorrectSteps4 extends TakesCorrectSteps5 { this: TakesCorrectSteps.type =>
    implicit def moveToInit[T <: HTree, S, L <: St.Label](
      implicit @evidence init: St.Api.Initial[L#Res, S]
    ): TakesCorrectSteps[St.Move[S, L] :: HNil, L] = mk
  }

  private[HTreeOps] sealed abstract class TakesCorrectSteps5 extends TakesCorrectSteps6 { this: TakesCorrectSteps.type =>
    implicit def fork[LHS <: HTree, RHS <: HTree, L <: St.Label](
      implicit
      @evidence lhs: TakesCorrectSteps[LHS, L],
      @evidence rhs: TakesCorrectSteps[RHS, L]
    ): TakesCorrectSteps[LHS :+: RHS, L] = mk
  }

  private[HTreeOps] sealed abstract class TakesCorrectSteps6 { this: TakesCorrectSteps.type =>
    implicit def end[L <: St.Label]: TakesCorrectSteps[HNil, L] =
      mk
  }
}
