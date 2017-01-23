package com.example

import fs2.Task
import fs2.interop.cats._

object doorWithIxFree extends App {

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

  val interp: FunctionX[DoorOp, IxMonad.Fake[Task]#λ] = new FunctionX[DoorOp, IxMonad.Fake[Task]#λ] {
    override def apply[F, T, A](d: DoorOp[F, T, A]): Task[A] = d match {
      case OpenDoor => Task.delay(println("Opening door ..."))
      case CloseDoor => Task.delay(println("Closing door ..."))
      case Knock => Task.delay(println("Knock-knock!"))
    }
  }

  val prog: DoorSm[Closed, Closed, Int] = for {
    _ <- knock
    _ <- open
    _ <- close
  } yield 8

  val tsk: Task[Int] =
    prog.foldMap[IxMonad.Fake[Task]#λ](interp)(IxMonad.ixMonadFromMonad[Task])

  println(tsk.unsafeRun())
}

object doorWithSm extends App {

  sealed trait Open
  sealed trait Closed

  sealed trait DoorOp[F, T, A]
  object DoorOp {
    implicit val init: Sm.Initial[DoorOp, Closed] =
      new Sm.Initial[DoorOp, Closed] {}
    implicit val fin: Sm.Final[DoorOp, Closed] =
      new Sm.Final[DoorOp, Closed] {}
  }
  final case object OpenDoor extends DoorOp[Closed, Open, Unit]
  final case object CloseDoor extends DoorOp[Open, Closed, Unit]
  final case object Knock extends DoorOp[Closed, Closed, Unit]

  type DoorSm[F, T, A] = Sm[DoorOp, F, T, A]

  val open: DoorSm[Closed, Open, Unit] =
    Sm.liftF(OpenDoor)
  val close: DoorSm[Open, Closed, Unit] =
    Sm.liftF(CloseDoor)
  val knock: DoorSm[Closed, Closed, Unit] =
    Sm.liftF(Knock)

  val interp: Sm.Interpreter[DoorOp, Task] = new Sm.Interpreter[DoorOp, Task] {
    override def apply[F, T, A](d: DoorOp[F, T, A]): Task[A] = d match {
      case OpenDoor => Task.delay(println("Opening door ..."))
      case CloseDoor => Task.delay(println("Closing door ..."))
      case Knock => Task.delay(println("Knock-knock!"))
    }
  }

  val prog: DoorSm[Closed, Closed, Int] = for {
    _ <- knock
    _ <- open
    _ <- close
  } yield 8

  val tsk: Task[Int] = prog.run(interp)

  println(tsk.unsafeRun())
}
