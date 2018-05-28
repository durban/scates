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
import cats.data.IndexedStateT
import cats.effect.IO

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.{ FlatSpec, Matchers }

object StackSafetySpec {
  final case class St(n: Int)
  sealed trait Op[F, T, A]
  final case object DoIt extends Op[St, St, Unit]
}

class StackSafetySpec extends FlatSpec with Matchers with TypeCheckedTripleEquals {

  import StackSafetySpec._

  final val N = 100000

  val prog: IxFree[Op, St, St, Unit] = (0 until N).foldLeft(IxFree.pure[Op, St, Unit](())) {
    (fu, i) => fu.flatMap { u =>
      IxFree.liftF[Op, St, St, Unit](DoIt)
    }
  }

  "IxFree#foldMap" should "be stack-safe" in {
    val r = prog.foldMap[IxMonad.Fake[Id]#λ](new FunctionX[Op, IxMonad.Fake[Id]#λ] {
      override def apply[A, B, C](f: Op[A, B, C]): C = f match {
        case DoIt =>
          ()
      }
    })(IxMonad.ixMonadFromMonad[Id])

    r should === (())
  }

  "IxMonad instance for IndexedStateT" should "be stack-safe (if F is stack-safe)" in {
    val r = prog.foldMap[IndexedStateT[IO, ?, ?, ?]](new FunctionX[Op, IndexedStateT[IO, ?, ?, ?]] {
      override def apply[A, B, C](f: Op[A, B, C]): IndexedStateT[IO, A, B, C] = f match {
        case DoIt =>
          IndexedStateT[IO, St, St, Unit] {
            case St(n) => IO.pure((St(n + 1), ()))
          }
      }
    })(IxMonad.ixMonadForIndexedStateT[IO])

    r.run(St(0)).unsafeRunSync() should === ((St(N), ()))
  }
}
