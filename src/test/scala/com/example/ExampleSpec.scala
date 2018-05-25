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

class ExampleSpec extends FlatSpec with Matchers with TypeCheckedTripleEquals {

  val ixFreeExamples: Map[IxFreeExample, Any] = Map(
    doorWithIxFree -> 8,
    atmWithIxFree -> ((Some(42), None))
  )

  val smExamples: Map[SmExample, Any] = Map(
    doorWithSm -> 8,
    atmCh -> 99,
    atm -> ((Some(56), None)),
    atmRes -> ((Some(21), None)),
    varRef -> 56.5
  )

  "IxFree examples" should "work" in {
    for ((ex, expResult) <- ixFreeExamples) {
      ex.tsk.unsafeRunSync() should === (expResult)
    }
  }

  "Sm examples" should "work" in {
    for ((ex, expResult) <- smExamples) {
      ex.tsk.unsafeRunSync() should === (expResult)
    }
  }
}
