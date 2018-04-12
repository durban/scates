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

import shapeless.test.illTyped

import HTreeOps._

final object HTreeOpsSpec {

  final class Rs1[A]

  final object Rs1 {

    implicit def initInt: St.Api.Initial[Rs1, Int] =
      new St.Api.Initial[Rs1, Int] {}

    implicit def intToByte: St.Api.Transition[Rs1, Int, Byte] =
      new St.Api.Transition[Rs1, Int, Byte] {}
  }

  final class Rs2[A]

  val l: St.Label[Rs1] { type Lb = this.type } = null
  val m: St.Label[Rs2] { type Lb = this.type } = null

  final object filter {

    val f1 = Filter[HNil, Rs1, l.Lb]
    implicitly[f1.Out =:= HNil]
    implicitly[f1.Rest =:= HNil]

    val f2 = Filter[St.Move[Rs1, Int, l.Lb] :: HNil, Rs1, l.Lb]
    implicitly[f2.Out =:= (St.Move[Rs1, Int, l.Lb] :: HNil)]
    implicitly[f2.Rest =:= HNil]

    val f3 = Filter[St.Move[Rs2, Int, m.Lb] :: HNil, Rs1, l.Lb]
    implicitly[f3.Out =:= HNil]
    implicitly[f3.Rest =:= (St.Move[Rs2, Int, m.Lb] :: HNil)]

    val f4 = Filter[St.Move[Rs2, Int, m.Lb] :: St.Move[Rs1, Int, l.Lb] :: HNil, Rs1, l.Lb]
    implicitly[f4.Out =:= (St.Move[Rs1, Int, l.Lb] :: HNil)]
    implicitly[f4.Rest =:= (St.Move[Rs2, Int, m.Lb] :: HNil)]

    val f5 = Filter[St.Move[Rs1, Float, l.Lb] :: St.Move[Rs2, Int, m.Lb] :: St.Move[Rs1, Int, l.Lb] :: HNil, Rs1, l.Lb]
    implicitly[f5.Out =:= (St.Move[Rs1, Float, l.Lb] :: St.Move[Rs1, Int, l.Lb] :: HNil)]
    implicitly[f5.Rest =:= (St.Move[Rs2, Int, m.Lb] :: HNil)]

    val f6 = Filter[St.InState[Rs1, Int, l.Lb] :: HNil, Rs1, l.Lb]
    implicitly[f6.Out =:= (St.InState[Rs1, Int, l.Lb] :: HNil)]
    implicitly[f6.Rest =:= HNil]

    val f7 = Filter[St.InState[Rs2, Int, m.Lb] :: HNil, Rs1, l.Lb]
    implicitly[f7.Out =:= HNil]
    implicitly[f7.Rest =:= (St.InState[Rs2, Int, m.Lb] :: HNil)]

    val f8 = Filter[St.Move[Rs2, Int, m.Lb] :: St.InState[Rs1, Int, l.Lb] :: HNil, Rs1, l.Lb]
    implicitly[f8.Out =:= (St.InState[Rs1, Int, l.Lb] :: HNil)]
    implicitly[f8.Rest =:= (St.Move[Rs2, Int, m.Lb] :: HNil)]

    val f9 = Filter[St.Move[Rs1, Float, l.Lb] :: St.Move[Rs2, Int, m.Lb] :: St.InState[Rs1, Int, l.Lb] :: HNil, Rs1, l.Lb]
    implicitly[f9.Out =:= (St.Move[Rs1, Float, l.Lb] :: St.InState[Rs1, Int, l.Lb] :: HNil)]
    implicitly[f9.Rest =:= (St.Move[Rs2, Int, m.Lb] :: HNil)]

    val f10 = Filter[HNil :+: HNil, Rs1, l.Lb]
    implicitly[f10.Out =:= (HNil :+: HNil)]
    implicitly[f10.Rest =:= (HNil :+: HNil)]

    val f11 = Filter[(St.Move[Rs1, Int, l.Lb] :: HNil) :+: HNil, Rs1, l.Lb]
    implicitly[f11.Out =:= ((St.Move[Rs1, Int, l.Lb] :: HNil) :+: HNil)]
    implicitly[f11.Rest =:= (HNil :+: HNil)]

    val f12 = Filter[(St.Move[Rs2, Int, m.Lb] :: St.Move[Rs1, Int, l.Lb] :: HNil) :+: HNil, Rs1, l.Lb]
    implicitly[f12.Out =:= ((St.Move[Rs1, Int, l.Lb] :: HNil) :+: HNil)]
    implicitly[f12.Rest =:= ((St.Move[Rs2, Int, m.Lb] :: HNil) :+: HNil)]

    val f13 = Filter[(St.Move[Rs2, Int, m.Lb] :: St.InState[Rs1, Int, l.Lb] :: HNil) :+: HNil, Rs1, l.Lb]
    implicitly[f13.Out =:= ((St.InState[Rs1, Int, l.Lb] :: HNil) :+: HNil)]
    implicitly[f13.Rest =:= ((St.Move[Rs2, Int, m.Lb] :: HNil) :+: HNil)]

    val f14 = Filter[(St.Move[Rs2, Int, m.Lb] :: St.InState[Rs1, Int, l.Lb] :: HNil) :+: (St.Move[Rs1, Float, l.Lb] :: HNil), Rs1, l.Lb]
    implicitly[f14.Out =:= ((St.InState[Rs1, Int, l.Lb] :: HNil) :+: (St.Move[Rs1, Float, l.Lb] :: HNil))]
    implicitly[f14.Rest =:= ((St.Move[Rs2, Int, m.Lb] :: HNil) :+: HNil)]

    val f15 = Filter[(St.Move[Rs2, Int, m.Lb] :: St.InState[Rs1, Int, l.Lb] :: HNil) :+: (St.InState[Rs2, Boolean, m.Lb] :: St.Move[Rs1, Float, l.Lb] :: HNil), Rs1, l.Lb]
    implicitly[f15.Out =:= ((St.InState[Rs1, Int, l.Lb] :: HNil) :+: (St.Move[Rs1, Float, l.Lb] :: HNil))]
    implicitly[f15.Rest =:= ((St.Move[Rs2, Int, m.Lb] :: HNil) :+: (St.InState[Rs2, Boolean, m.Lb] :: HNil))]

    val f16 = Filter[
      (
        (
          (St.InState[Rs1, Float, l.Lb] :: St.Move[Rs1, Float, l.Lb] :: HNil) :+:
          (St.Move[Rs1, Float, l.Lb] :: St.Move[Rs2, Double, m.Lb] :: HNil)
        ) :+:
        (
          (St.InState[Rs2, Float, m.Lb] :: St.Move[Rs2, Float, m.Lb] :: St.Move[Rs1, Float, l.Lb] :: HNil) :+:
          (St.InState[Rs2, Float, m.Lb] :: HNil)
        )
      ),
      Rs1,
      l.Lb
    ]
    implicitly[
      f16.Out =:=
      (
        (
          (St.InState[Rs1, Float, l.Lb] :: St.Move[Rs1, Float, l.Lb] :: HNil) :+:
          (St.Move[Rs1, Float, l.Lb] :: HNil)
        ) :+:
        (
          (St.Move[Rs1, Float, l.Lb] :: HNil) :+:
          (HNil)
        )
      )
    ]
    implicitly[
      f16.Rest =:=
      (
        (
          (HNil) :+:
          (St.Move[Rs2, Double, m.Lb] :: HNil)
        ) :+:
        (
          (St.InState[Rs2, Float, m.Lb] :: St.Move[Rs2, Float, m.Lb] :: HNil) :+:
          (St.InState[Rs2, Float, m.Lb] :: HNil)
        )
      )
    ]
  }

  final object destroyedAtEnd {

    val d1 = DestroyedAtEnd[St.Delete[Rs1, l.Lb] :: HNil, Rs1, l.Lb]
    implicitly[d1.Out =:= HNil]

    val d2 = DestroyedAtEnd[St.Delete[Rs1, l.Lb] :: St.Move[Rs1, Int, l.Lb] :: HNil, Rs1, l.Lb]
    implicitly[d2.Out =:= (St.Move[Rs1, Int, l.Lb] :: HNil)]

    val d3 = DestroyedAtEnd[(St.Delete[Rs1, l.Lb] :: St.Move[Rs1, Int, l.Lb] :: HNil) :+: (St.Delete[Rs1, l.Lb] :: HNil), Rs1, l.Lb]
    implicitly[d3.Out =:= ((St.Move[Rs1, Int, l.Lb] :: HNil) :+: HNil)]

    illTyped("""DestroyedAtEnd[HNil, Rs1, l.Lb]""", "could not find implicit value for parameter.*")
    illTyped("""DestroyedAtEnd[St.Move[Rs1, Int, l.Lb] :: HNil, Rs1, l.Lb]""", "could not find implicit value for parameter.*")
    illTyped("""DestroyedAtEnd[St.InState[Rs1, Int, l.Lb] :: HNil, Rs1, l.Lb]""", "could not find implicit value for parameter.*")
    illTyped("""DestroyedAtEnd[(St.Move[Rs1, Int, l.Lb] :: HNil) :+: (St.Delete[Rs1, l.Lb] :: HNil), Rs1, l.Lb]""", "could not find implicit value for parameter.*")
    illTyped("""DestroyedAtEnd[((St.Delete[Rs1, l.Lb] :: HNil) :+: (St.Move[Rs1, Int, l.Lb] :: HNil)) :+: (St.Delete[Rs1, l.Lb] :: HNil), Rs1, l.Lb]""", "could not find implicit value for parameter.*")
  }

  final object takesCorrectSteps {

    TakesCorrectSteps[HNil, Rs1, l.Lb]
    TakesCorrectSteps[St.Move[Rs1, Int, l.Lb] :: HNil, Rs1, l.Lb]
    illTyped("""TakesCorrectSteps[St.Move[Rs1, Float, l.Lb] :: HNil, Rs1, l.Lb]""", "could not find implicit value for parameter.*")
    illTyped("""TakesCorrectSteps[St.InState[Rs1, Int, l.Lb] :: HNil, Rs1, l.Lb]""", "could not find implicit value for parameter.*")
    illTyped("""TakesCorrectSteps[St.InState[Rs1, Byte, l.Lb] :: St.InState[Rs1, Int, l.Lb] :: HNil, Rs1, l.Lb]""", "could not find implicit value for parameter.*")
    illTyped("""TakesCorrectSteps[(HNil) :+: (St.InState[Rs1, Byte, l.Lb] :: St.InState[Rs1, Int, l.Lb] :: HNil), Rs1, l.Lb]""", "could not find implicit value for parameter.*")

    TakesCorrectSteps[St.InState[Rs1, Int, l.Lb] :: St.Move[Rs1, Int, l.Lb] :: HNil, Rs1, l.Lb]
    illTyped("""TakesCorrectSteps[St.InState[Rs1, Float, l.Lb] :: St.Move[Rs1, Int, l.Lb] :: HNil, Rs1, l.Lb]""", "could not find implicit value for parameter.*")

    TakesCorrectSteps[St.Move[Rs1, Byte, l.Lb] :: St.Move[Rs1, Int, l.Lb] :: HNil, Rs1, l.Lb]
    illTyped("""TakesCorrectSteps[St.Move[Rs1, Float, l.Lb] :: St.Move[Rs1, Int, l.Lb] :: HNil, Rs1, l.Lb]""", "could not find implicit value for parameter.*")

    TakesCorrectSteps[St.Move[Rs1, Byte, l.Lb] :: St.InState[Rs1, Int, l.Lb] :: St.Move[Rs1, Int, l.Lb] :: HNil, Rs1, l.Lb]
    illTyped("""TakesCorrectSteps[St.Move[Rs1, Float, l.Lb] :: St.InState[Rs1, Int, l.Lb] :: St.Move[Rs1, Int, l.Lb] :: HNil, Rs1, l.Lb]""", "could not find implicit value for parameter.*")

    TakesCorrectSteps[St.InState[Rs1, Int, l.Lb] :: St.InState[Rs1, Int, l.Lb] :: St.Move[Rs1, Int, l.Lb] :: HNil, Rs1, l.Lb]
    illTyped("""TakesCorrectSteps[St.InState[Rs1, Byte, l.Lb] :: St.InState[Rs1, Int, l.Lb] :: St.Move[Rs1, Int, l.Lb] :: HNil, Rs1, l.Lb]""", "could not find implicit value for parameter.*")

    TakesCorrectSteps[HNil :+: HNil, Rs1, l.Lb]
    TakesCorrectSteps[(St.Move[Rs1, Int, l.Lb] :: HNil) :+: HNil, Rs1, l.Lb]
    TakesCorrectSteps[(St.Move[Rs1, Int, l.Lb] :: HNil) :+: (St.InState[Rs1, Int, l.Lb] :: St.Move[Rs1, Int, l.Lb] :: HNil), Rs1, l.Lb]
    illTyped("""TakesCorrectSteps[HNil :+: (St.Move[Rs1, Float, l.Lb] :: HNil), Rs1, l.Lb]""", "could not find implicit value for parameter.*")
    illTyped("""TakesCorrectSteps[HNil :+: (St.InState[Rs1, Int, l.Lb] :: HNil), Rs1, l.Lb]""", "could not find implicit value for parameter.*")
    illTyped("""TakesCorrectSteps[(St.Move[Rs1, Int, l.Lb] :: HNil) :+: (St.InState[Rs1, Byte, l.Lb] :: St.InState[Rs1, Int, l.Lb] :: HNil), Rs1, l.Lb]""", "could not find implicit value for parameter.*")

    TakesCorrectSteps[(St.InState[Rs1, Int, l.Lb] :: St.Move[Rs1, Int, l.Lb] :: HNil) :+: (St.InState[Rs1, Int, l.Lb] :: St.InState[Rs1, Int, l.Lb] :: St.Move[Rs1, Int, l.Lb] :: HNil), Rs1, l.Lb]
    illTyped("""TakesCorrectSteps[(St.InState[Rs1, Float, l.Lb] :: St.Move[Rs1, Int, l.Lb] :: HNil) :+: (St.InState[Rs1, Int, l.Lb] :: St.InState[Rs1, Int, l.Lb] :: St.Move[Rs1, Int, l.Lb] :: HNil), Rs1, l.Lb]""", "could not find implicit value for parameter.*")
  }

  final object consistentResource {
    ConsistentResource[St.Delete[Rs1, l.Lb] :: HNil, Rs1, l.Lb]
    ConsistentResource[(St.Delete[Rs1, l.Lb] :: HNil) :+: (St.Delete[Rs1, l.Lb] :: St.Move[Rs1, Int, l.Lb] :: HNil), Rs1, l.Lb]
    illTyped("""ConsistentResource[(St.Delete[Rs1, l.Lb] :: HNil) :+: (St.Delete[Rs1, l.Lb] :: St.InState[Rs1, Int, l.Lb] :: HNil), Rs1, l.Lb]""", "could not find implicit value for parameter.*")
  }
}
