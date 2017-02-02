package com.example

import fs2.Task
import fs2.interop.cats._
import fs2.interop.scalaz._

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
    implicit val init: Sm.Initial.Aux[DoorOp, Closed] =
      Sm.Initial.define
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

  implicit val interp: Sm.Execute.Aux[DoorOp, Task, Const[Null]#λ] = new Sm.Execute[DoorOp, Task] {
    override type Res[st] = Null
    override def exec[F, T, A](res: Null)(d: DoorOp[F, T, A]): Task[(Null, A)] = d match {
      case OpenDoor => Task.delay((res, println("Opening door ...")))
      case CloseDoor => Task.delay((res, println("Closing door ...")))
      case Knock => Task.delay((res, println("Knock-knock!")))
    }
  }

  implicit val mk: Sm.Create.Aux[DoorOp, Const[Null]#λ, Closed] =
    Sm.Create.instance[DoorOp, Const[Null]#λ, Closed](null)

  val prog: DoorSm[Closed, Closed, Int] = for {
    _ <- knock
    _ <- open
    _ <- close
  } yield 8

  val tsk: Task[Int] = prog.run[Task, Const[Null]#λ]

  println(tsk.unsafeRun())
}

/** From http://alcestes.github.io/lchannels/ */
object atm extends App {

  sealed trait NotAuthenticated
  sealed trait Authenticated

  sealed trait AtmOp[F, T, A]
  object AtmOp {
    implicit val init: Sm.Initial.Aux[AtmOp, NotAuthenticated] =
      Sm.Initial.define
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

  implicit val interp: Sm.Execute.Aux[AtmOp, Task, Const[Null]#λ] = new Sm.Execute[AtmOp, Task] {
    override type Res[st] = Null
    override def exec[F, T, A](res: Null)(d: AtmOp[F, T, A]): Task[(Null, A)] = d match {
      case Authenticate(c, p) =>
        if (p == "1234") {
          Task.delay((res, println(s"Card ${c} accepted")))
        } else {
          Task.fail(new Exception("authentication error!!!"))
        }
      case CheckBalance =>
        Task.delay {
          println("Returning balance")
          (res, 56)
        }
      case Quit => Task.delay((res, println("Goodbye")))
    }
  }

  implicit val mk: Sm.Create.Aux[AtmOp, Const[Null]#λ, NotAuthenticated] =
    Sm.Create.instance[AtmOp, Const[Null]#λ, NotAuthenticated](null)

  val goodPin: AtmSm[NotAuthenticated, NotAuthenticated, Int] = for {
    _ <- authenticate("11232432", "1234")
    b <- checkBalance
  } yield b

  val badPin: AtmSm[NotAuthenticated, NotAuthenticated, Int] = for {
    _ <- authenticate("11232432", "9876")
    b <- checkBalance
  } yield b

  val tsk1: Task[Int] = goodPin.run[Task, Const[Null]#λ]
  val tsk2: Task[Int] = badPin.run[Task, Const[Null]#λ]

  println(tsk1.unsafeRun())
  println(tsk2.unsafeRun())
}

/** From Idris States */
object varRef extends App {

  sealed trait VarOp[F, T, A]
  object VarOp {
    implicit val init: Sm.Initial.Aux[VarOp, Unit] =
      Sm.Initial.define
    implicit def fin[A]: Sm.Final[VarOp, A] =
      new Sm.Final[VarOp, A] {}
  }
  final case class Get[A]() extends VarOp[A, A, A]
  final case class Put[F, T](t: T) extends VarOp[F, T, Unit]

  type VarSm[F, T, A] = Sm[VarOp, F, T, A]

  def get[A]: VarSm[A, A, A] = Sm.liftF(Get())
  def put[F, T](t: T): VarSm[F, T, Unit] = Sm.liftF(Put(t))

  implicit val interp: Sm.Execute.Aux[VarOp, Task, cats.Id] = new Sm.Execute[VarOp, Task] {
    override type Res[st] = st
    override def exec[F, T, A](res: F)(d: VarOp[F, T, A]): Task[(T, A)] = d match {
      case g: Get[a] => Task.now((res, res))
      case p: Put[f, t] => Task.now((p.t, ()))
    }
  }

  implicit val mk: Sm.Create.Aux[VarOp, cats.Id, Unit] =
    Sm.Create.instance[VarOp, cats.Id, Unit](())

  val prog: VarSm[Unit, Double, Double] = for {
    _ <- Sm.pure[VarOp, Unit](())
    _ <- put(56)
    _ <- put(true)
    b <- get
    _ <- put(56.5)
    f <- get
  } yield if (b) f else 4.5

  println(prog.run[Task, cats.Id].unsafeRun())
}
