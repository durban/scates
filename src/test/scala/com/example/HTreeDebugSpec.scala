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

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.Matchers
import org.scalatest.FlatSpec

class HTreeDebugSpec extends FlatSpec with Matchers with TypeCheckedTripleEquals {

  import HTree._

  final class Rs1[A]

  final object Rs1 {
    implicit def initInt: St.Api.Initial[Rs1, Int] =
      new St.Api.Initial[Rs1, Int] {}
  }

  val l = St.create[Rs1, Int].run.unsafeRun.unsafeRunSync()
  val lt = Trace[l.type].trace

  val m = St.create[Rs1, Int].run.unsafeRun.unsafeRunSync()
  val mt = Trace[m.type].trace

  "HTree.Debug" should "work correctly" in {
    lt should not be (mt)
    Debug[HNil].debug should === ("HNil")
    Debug[St.Move[Int, l.type] :: HNil].debug should === (s"Move[$lt] :: HNil")
    Debug[St.InState[Int, l.type] :: St.Move[Int, l.type] :: HNil].debug should === (s"Read[$lt] :: Move[$lt] :: HNil")

    Debug[HNil :+: HNil].debug should === (s"(HNil) :+: (HNil)")
    Debug[(St.Move[Int, l.type] :: HNil) :+: HNil].debug should === (s"(Move[$lt] :: HNil) :+: (HNil)")

    Debug[St.Move[Int, m.type] :: St.Move[Int, l.type] :: HNil].debug should === (s"Move[$mt] :: Move[$lt] :: HNil")
    Debug[St.Move[Int, l.type] :: St.Move[Int, m.type] :: HNil].debug should === (s"Move[$lt] :: Move[$mt] :: HNil")
  }
}
