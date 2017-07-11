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

import cats.Id

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.{ FlatSpec, Matchers }

object StackSafetySpec {
  sealed trait St
  sealed trait Op[F, T, A]
  final case object DoIt extends Op[St, St, Unit]
}

class StackSafetySpec extends FlatSpec with Matchers with TypeCheckedTripleEquals {

  import StackSafetySpec._

  // TODO: make it pass
  "IxFree#foldMap" should "be stack-safe" ignore {
    val prog: IxFree[Op, St, St, Unit] = (0 until 100000).foldLeft(IxFree.pure[Op, St, Unit](())) {
      (fu, i) => fu.flatMap { u =>
        IxFree.liftF[Op, St, St, Unit](DoIt)
      }
    }

    val r = prog.foldMap[IxMonad.Fake[Id]#λ](new FunctionX[Op, IxMonad.Fake[Id]#λ] {
      override def apply[A, B, C](f: Op[A, B, C]): C = f match {
        case DoIt =>
          ()
      }
    })(IxMonad.ixMonadFromMonad[Id])

    r should === (())
  }
}
