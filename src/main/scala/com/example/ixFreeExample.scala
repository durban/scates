/*
 * Copyright 2016-2018 Daniel Urban and contributors listed in AUTHORS
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

import cats.effect.IO

sealed trait IxFreeExample extends AbstractExample {
  def tsk: IO[Any]
}

/** From Idris States */
object doorWithIxFree extends IxFreeExample {

  sealed trait Open
  sealed trait Closed

  sealed trait DoorOp[F, T, A]
  final case object OpenDoor extends DoorOp[Closed, Open, Unit]
  final case object CloseDoor extends DoorOp[Open, Closed, Unit]
  final case object Knock extends DoorOp[Closed, Closed, Unit]

  type DoorSm[F, T, A] = IxFree[DoorOp, F, T, A]

  val open: DoorSm[Closed, Open, Unit] =
    IxFree.liftF(OpenDoor)
  val close: DoorSm[Open, Closed, Unit] =
    IxFree.liftF(CloseDoor)
  val knock: DoorSm[Closed, Closed, Unit] =
    IxFree.liftF(Knock)

  val interp: FunctionX[DoorOp, IxMonad.Fake[IO]#λ] = new FunctionX[DoorOp, IxMonad.Fake[IO]#λ] {
    override def apply[F, T, A](d: DoorOp[F, T, A]): IO[A] = d match {
      case OpenDoor => IO(println("Opening door ..."))
      case CloseDoor => IO(println("Closing door ..."))
      case Knock => IO(println("Knock-knock!"))
    }
  }

  val prog: DoorSm[Closed, Closed, Int] = for {
    _ <- knock
    _ <- open
    _ <- close
  } yield 8

  val tsk: IO[Int] =
    prog.foldMap[IxMonad.Fake[IO]#λ](interp)(IxMonad.ixMonadFromMonad[IO])
}
