package com.example

import fs2.Task
import fs2.interop.cats._

/** From Idris States */
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

/** From Idris States */
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

/** From http://alcestes.github.io/lchannels/ */
object atm extends App {
  
  sealed trait NotAuthenticated
  sealed trait Authenticated
  
  sealed trait AtmOp[F, T, A]
  object AtmOp {
    implicit val init: Sm.Initial[AtmOp, NotAuthenticated] =
      new Sm.Initial[AtmOp, NotAuthenticated] {}
    implicit val fin: Sm.Final[AtmOp, NotAuthenticated] =
      new Sm.Final[AtmOp, NotAuthenticated] {}
  }
  final case class Authenticate(card: String, pin: String)
    extends AtmOp[NotAuthenticated, Authenticated, Unit]
  final case object CheckBalance
    extends AtmOp[Authenticated, NotAuthenticated, Int]
  final case object Quit
    extends AtmOp[Authenticated, NotAuthenticated, Unit]
  
  type AtmSm[F, T, A] = Sm[AtmOp, F, T, A]
  
  def authenticate(card: String, pin: String): AtmSm[NotAuthenticated, Authenticated, Unit] =
    Sm.liftF(Authenticate(card, pin))
  val checkBalance: AtmSm[Authenticated, NotAuthenticated, Int] =
    Sm.liftF(CheckBalance)
  val quit: AtmSm[Authenticated, NotAuthenticated, Unit] =
    Sm.liftF(Quit)
  
  val interp: Sm.Interpreter[AtmOp, Task] = new Sm.Interpreter[AtmOp, Task] {
    override def apply[F, T, A](d: AtmOp[F, T, A]): Task[A] = d match {
      case Authenticate(c, p) =>
        if (p == "1234") {
          Task.delay(println(s"Card ${c} accepted"))
        } else {
          Task.fail(new Exception("authentication error!!!"))
        }
      case CheckBalance =>
        Task.delay {
          println("Returning balance")
          56
        }
      case Quit => Task.delay(println("Goodbye"))
    }
  }
    
  val goodPin: AtmSm[NotAuthenticated, NotAuthenticated, Int] = for {
    _ <- authenticate("11232432", "1234")
    b <- checkBalance
  } yield b

  val badPin: AtmSm[NotAuthenticated, NotAuthenticated, Int] = for {
    _ <- authenticate("11232432", "9876")
    b <- checkBalance
  } yield b
  
  val tsk1: Task[Int] = goodPin.run(interp)
  val tsk2: Task[Int] = badPin.run(interp)
  
  println(tsk1.unsafeRun())
  println(tsk2.unsafeRun())
}
