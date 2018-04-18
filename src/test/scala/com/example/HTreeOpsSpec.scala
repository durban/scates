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

import scala.language.existentials

import shapeless.test.illTyped

import HTreeOps._

final object HTreeOpsSpec {

  final class Rs1[A]

  final object Rs1 {

    implicit def initInt: St.Api.Initial[Rs1, Int] =
      new St.Api.Initial[Rs1, Int] {}

    implicit def intToByte: St.Api.Transition[Rs1, Int, Byte] =
      new St.Api.Transition[Rs1, Int, Byte] {}

    implicit def finInt: St.Api.Final[Rs1, Int] =
      new St.Api.Transition[Rs1, Int, St.Destroyed] {}
  }

  final class Rs2[A]

  final object Rs2 {
    implicit def initInt: St.Api.Initial[Rs2, Int] =
      new St.Api.Initial[Rs2, Int] {}
  }

  sealed trait Phantom

  val l = St.unsafeRun(St.create[Rs1, Int]).unsafeRunSync()
  implicitly[l.Res[Phantom] =:= Rs1[Phantom]]

  val m = St.unsafeRun(St.create[Rs2, Int]).unsafeRunSync()
  implicitly[m.Res[Phantom] =:= Rs2[Phantom]]

  final object filter {

    val f1 = Filter[HNil, l.type]
    implicitly[f1.Out =:= HNil]
    implicitly[f1.Rest =:= HNil]

    val f2 = Filter[St.Move[Int, l.type] :: HNil, l.type]
    implicitly[f2.Out =:= (St.Move[Int, l.type] :: HNil)]
    implicitly[f2.Rest =:= HNil]

    val f3 = Filter[St.Move[Int, m.type] :: HNil, l.type]
    implicitly[f3.Out =:= HNil]
    implicitly[f3.Rest =:= (St.Move[Int, m.type] :: HNil)]

    val f4 = Filter[St.Move[Int, m.type] :: St.Move[Int, l.type] :: HNil, l.type]
    implicitly[f4.Out =:= (St.Move[Int, l.type] :: HNil)]
    implicitly[f4.Rest =:= (St.Move[Int, m.type] :: HNil)]

    val f5 = Filter[St.Move[Float, l.type] :: St.Move[Int, m.type] :: St.Move[Int, l.type] :: HNil, l.type]
    implicitly[f5.Out =:= (St.Move[Float, l.type] :: St.Move[Int, l.type] :: HNil)]
    implicitly[f5.Rest =:= (St.Move[Int, m.type] :: HNil)]

    val f6 = Filter[St.InState[Int, l.type] :: HNil, l.type]
    implicitly[f6.Out =:= (St.InState[Int, l.type] :: HNil)]
    implicitly[f6.Rest =:= HNil]

    val f7 = Filter[St.InState[Int, m.type] :: HNil, l.type]
    implicitly[f7.Out =:= HNil]
    implicitly[f7.Rest =:= (St.InState[Int, m.type] :: HNil)]

    val f8 = Filter[St.Move[Int, m.type] :: St.InState[Int, l.type] :: HNil, l.type]
    implicitly[f8.Out =:= (St.InState[Int, l.type] :: HNil)]
    implicitly[f8.Rest =:= (St.Move[Int, m.type] :: HNil)]

    val f9 = Filter[St.Move[Float, l.type] :: St.Move[Int, m.type] :: St.InState[Int, l.type] :: HNil, l.type]
    implicitly[f9.Out =:= (St.Move[Float, l.type] :: St.InState[Int, l.type] :: HNil)]
    implicitly[f9.Rest =:= (St.Move[Int, m.type] :: HNil)]

    val f10 = Filter[HNil :+: HNil, l.type]
    implicitly[f10.Out =:= (HNil :+: HNil)]
    implicitly[f10.Rest =:= (HNil :+: HNil)]

    val f11 = Filter[(St.Move[Int, l.type] :: HNil) :+: HNil, l.type]
    implicitly[f11.Out =:= ((St.Move[Int, l.type] :: HNil) :+: HNil)]
    implicitly[f11.Rest =:= (HNil :+: HNil)]

    val f12 = Filter[(St.Move[Int, m.type] :: St.Move[Int, l.type] :: HNil) :+: HNil, l.type]
    implicitly[f12.Out =:= ((St.Move[Int, l.type] :: HNil) :+: HNil)]
    implicitly[f12.Rest =:= ((St.Move[Int, m.type] :: HNil) :+: HNil)]

    val f13 = Filter[(St.Move[Int, m.type] :: St.InState[Int, l.type] :: HNil) :+: HNil, l.type]
    implicitly[f13.Out =:= ((St.InState[Int, l.type] :: HNil) :+: HNil)]
    implicitly[f13.Rest =:= ((St.Move[Int, m.type] :: HNil) :+: HNil)]

    val f14 = Filter[(St.Move[Int, m.type] :: St.InState[Int, l.type] :: HNil) :+: (St.Move[Float, l.type] :: HNil), l.type]
    implicitly[f14.Out =:= ((St.InState[Int, l.type] :: HNil) :+: (St.Move[Float, l.type] :: HNil))]
    implicitly[f14.Rest =:= ((St.Move[Int, m.type] :: HNil) :+: HNil)]

    val f15 = Filter[(St.Move[Int, m.type] :: St.InState[Int, l.type] :: HNil) :+: (St.InState[Boolean, m.type] :: St.Move[Float, l.type] :: HNil), l.type]
    implicitly[f15.Out =:= ((St.InState[Int, l.type] :: HNil) :+: (St.Move[Float, l.type] :: HNil))]
    implicitly[f15.Rest =:= ((St.Move[Int, m.type] :: HNil) :+: (St.InState[Boolean, m.type] :: HNil))]

    val f16 = Filter[
      (
        (
          (St.InState[Float, l.type] :: St.Move[Float, l.type] :: HNil) :+:
          (St.Move[Float, l.type] :: St.Move[Double, m.type] :: HNil)
        ) :+:
        (
          (St.InState[Float, m.type] :: St.Move[Float, m.type] :: St.Move[Float, l.type] :: HNil) :+:
          (St.InState[Float, m.type] :: HNil)
        )
      ),
      l.type
    ]
    implicitly[
      f16.Out =:=
      (
        (
          (St.InState[Float, l.type] :: St.Move[Float, l.type] :: HNil) :+:
          (St.Move[Float, l.type] :: HNil)
        ) :+:
        (
          (St.Move[Float, l.type] :: HNil) :+:
          (HNil)
        )
      )
    ]
    implicitly[
      f16.Rest =:=
      (
        (
          (HNil) :+:
          (St.Move[Double, m.type] :: HNil)
        ) :+:
        (
          (St.InState[Float, m.type] :: St.Move[Float, m.type] :: HNil) :+:
          (St.InState[Float, m.type] :: HNil)
        )
      )
    ]
  }

  final object destroyedAtEnd {

    DestroyedAtEnd[St.Delete[l.type] :: HNil, l.type]
    DestroyedAtEnd[St.Delete[l.type] :: St.Move[Int, l.type] :: HNil, l.type]
    DestroyedAtEnd[(St.Delete[l.type] :: St.Move[Int, l.type] :: HNil) :+: (St.Delete[l.type] :: HNil), l.type]

    illTyped("""DestroyedAtEnd[HNil, l.type]""", "could not find implicit value for parameter.*")
    illTyped("""DestroyedAtEnd[St.Move[Int, l.type] :: HNil, l.type]""", "could not find implicit value for parameter.*")
    illTyped("""DestroyedAtEnd[St.InState[Int, l.type] :: HNil, l.type]""", "could not find implicit value for parameter.*")
    illTyped("""DestroyedAtEnd[(St.Move[Int, l.type] :: HNil) :+: (St.Delete[l.type] :: HNil), l.type]""", "could not find implicit value for parameter.*")
    illTyped("""DestroyedAtEnd[((St.Delete[l.type] :: HNil) :+: (St.Move[Int, l.type] :: HNil)) :+: (St.Delete[l.type] :: HNil), l.type]""", "could not find implicit value for parameter.*")
  }

  final object takesCorrectSteps {

    TakesCorrectSteps[HNil, l.type]
    TakesCorrectSteps[St.Move[Int, l.type] :: HNil, l.type]
    illTyped("""TakesCorrectSteps[St.Move[Float, l.type] :: HNil, l.type]""", "could not find implicit value for parameter.*")
    illTyped("""TakesCorrectSteps[St.InState[Int, l.type] :: HNil, l.type]""", "could not find implicit value for parameter.*")
    illTyped("""TakesCorrectSteps[St.InState[Byte, l.type] :: St.InState[Int, l.type] :: HNil, l.type]""", "could not find implicit value for parameter.*")
    illTyped("""TakesCorrectSteps[(HNil) :+: (St.InState[Byte, l.type] :: St.InState[Int, l.type] :: HNil), l.type]""", "could not find implicit value for parameter.*")

    TakesCorrectSteps[St.InState[Int, l.type] :: St.Move[Int, l.type] :: HNil, l.type]
    illTyped("""TakesCorrectSteps[St.InState[Float, l.type] :: St.Move[Int, l.type] :: HNil, l.type]""", "could not find implicit value for parameter.*")

    TakesCorrectSteps[St.Move[Byte, l.type] :: St.Move[Int, l.type] :: HNil, l.type]
    illTyped("""TakesCorrectSteps[St.Move[Float, l.type] :: St.Move[Int, l.type] :: HNil, l.type]""", "could not find implicit value for parameter.*")

    TakesCorrectSteps[St.Move[Byte, l.type] :: St.InState[Int, l.type] :: St.Move[Int, l.type] :: HNil, l.type]
    illTyped("""TakesCorrectSteps[St.Move[Float, l.type] :: St.InState[Int, l.type] :: St.Move[Int, l.type] :: HNil, l.type]""", "could not find implicit value for parameter.*")

    TakesCorrectSteps[St.InState[Int, l.type] :: St.InState[Int, l.type] :: St.Move[Int, l.type] :: HNil, l.type]
    illTyped("""TakesCorrectSteps[St.InState[Byte, l.type] :: St.InState[Int, l.type] :: St.Move[Int, l.type] :: HNil, l.type]""", "could not find implicit value for parameter.*")

    TakesCorrectSteps[HNil :+: HNil, l.type]
    TakesCorrectSteps[(St.Move[Int, l.type] :: HNil) :+: HNil, l.type]
    TakesCorrectSteps[(St.Move[Int, l.type] :: HNil) :+: (St.InState[Int, l.type] :: St.Move[Int, l.type] :: HNil), l.type]
    illTyped("""TakesCorrectSteps[HNil :+: (St.Move[Float, l.type] :: HNil), l.type]""", "could not find implicit value for parameter.*")
    illTyped("""TakesCorrectSteps[HNil :+: (St.InState[Int, l.type] :: HNil), l.type]""", "could not find implicit value for parameter.*")
    illTyped("""TakesCorrectSteps[(St.Move[Int, l.type] :: HNil) :+: (St.InState[Byte, l.type] :: St.InState[Int, l.type] :: HNil), l.type]""", "could not find implicit value for parameter.*")

    TakesCorrectSteps[(St.InState[Int, l.type] :: St.Move[Int, l.type] :: HNil) :+: (St.InState[Int, l.type] :: St.InState[Int, l.type] :: St.Move[Int, l.type] :: HNil), l.type]
    illTyped("""TakesCorrectSteps[(St.InState[Float, l.type] :: St.Move[Int, l.type] :: HNil) :+: (St.InState[Int, l.type] :: St.InState[Int, l.type] :: St.Move[Int, l.type] :: HNil), l.type]""", "could not find implicit value for parameter.*")
  }

  final object consistentResource {
    ConsistentResource[St.Delete[l.type] :: St.Move[Int, l.type] :: HNil, l.type]
    ConsistentResource[(St.Delete[l.type] :: St.Move[Int, l.type] :: HNil) :+: (St.Delete[l.type] :: St.InState[Int, l.type] :: St.Move[Int, l.type] :: HNil), l.type]
    illTyped("""ConsistentResource[(St.Delete[l.type] :: St.Move[Int, l.type] :: HNil) :+: (St.Delete[l.type] :: St.Move[Int, l.type] :: St.InState[Int, l.type] :: HNil), l.type]""", "could not find implicit value for parameter.*")
  }

  final object headResource {

    val h1 = HeadResource[St.Delete[l.type] :: HNil]
    implicitly[h1.L =:= l.type]

    val h2 = HeadResource[St.Move[Int, l.type] :: HNil]
    implicitly[h2.L =:= l.type]

    val h3 = HeadResource[St.InState[Int, l.type] :: HNil]
    implicitly[h3.L =:= l.type]

    val h4 = HeadResource[St.Delete[l.type] :: St.InState[Int, l.type] :: St.Move[Int, l.type] :: HNil]
    implicitly[h4.L =:= l.type]
  }

  final object consistentTree {

    ConsistentTree[HNil]

    val hr = HeadResource[St.Delete[l.type] :: St.Move[Int, l.type] :: HNil]
    implicitly[hr.L =:= l.type]
    val fl = Filter[St.Delete[l.type] :: St.Move[Int, l.type] :: HNil, hr.L]
    implicitly[fl.Out =:= (St.Delete[l.type] :: St.Move[Int, l.type] :: HNil)]
    val rs = ConsistentResource[fl.Out, hr.L]
    ConsistentResource[(St.Delete[l.type] :: St.Move[Int, l.type] :: HNil), hr.L]

    ConsistentTree[St.Delete[l.type] :: St.Move[Int, l.type] :: HNil]
  }
}
