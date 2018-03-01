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

  println(tsk.unsafeRunSync())
}

/** From Idris States */
object doorWithSm extends App {

  sealed trait Open
  sealed trait Closed

  sealed trait DoorOp[F, T, A]
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

  implicit val interp: Sm.Execute.Aux[DoorOp, IO, Const[Unit]#λ, Closed, Closed] = new Sm.Execute[DoorOp] {
    override type M[a] = IO[a]
    override type InitSt = Closed
    override type FinSt = Closed
    override type Res[st] = Unit
    override def init: IO[Unit] = IO.unit
    override def exec[F, T, A](res: Unit)(d: DoorOp[F, T, A]): IO[(Unit, A)] = d match {
      case OpenDoor => IO((res, println("Opening door ...")))
      case CloseDoor => IO((res, println("Closing door ...")))
      case Knock => IO((res, println("Knock-knock!")))
    }
    override def fin(ref: Res[Closed]): IO[Unit] = IO.unit
  }

  val prog: DoorSm[Closed, Closed, Int] = for {
    _ <- knock
    _ <- open
    _ <- close
  } yield 8

  val tsk: IO[Int] = prog.run[IO, Const[Unit]#λ]

  println(tsk.unsafeRunSync())
}

/** From http://alcestes.github.io/lchannels/ */
object atmCh extends App {

  import akka.typed._
  import akka.typed.scaladsl._
  import akka.typed.scaladsl.AskPattern._
  import scala.concurrent.duration._
  import scala.util.{ Success, Failure }
  import scala.concurrent.ExecutionContext.Implicits.global

  implicit val to: akka.util.Timeout =
    akka.util.Timeout(2.seconds)

  final case class ActRef[A](ref: ActorRef[A])(implicit sch: akka.actor.Scheduler) {
    val scheduler = sch
    def ! (a: A): IO[Unit] = IO { ref ! a }
    def ? [B](f: ActorRef[B] => A): IO[B] = IO.suspend {
      IO.async[B] { cb =>
        val fut = ref ? f
        fut.onComplete {
          case Success(b) => cb(Right(b))
          case Failure(ex) => cb(Left(ex))
        }
      }
    }
  }

  object msgs {

    case class Authenticate(card: String, pin: String)(val cont: ActorRef[Response])

    sealed abstract class Response
    case class Failure() extends Response
    case class Success()(val cont: ActorRef[Menu]) extends Response

    sealed abstract class Menu
    case class CheckBalance()(val cont: ActorRef[Balance]) extends Menu
    case class Quit() extends Menu

    case class Balance(amount: Int)
  }

  object client {

    sealed trait AtmOp[F, T, A]

    final case class Authenticate(card: String, pin: String)
      extends AtmOp[msgs.Authenticate, msgs.Menu, Unit]
    final case object CheckBalance
      extends AtmOp[msgs.Menu, msgs.Authenticate, Int]
    final case object Quit
      extends AtmOp[msgs.Menu, msgs.Authenticate, Unit]

    type AtmSm[F, T, A] = Sm[AtmOp, F, T, A]

    def authenticate(card: String, pin: String): AtmSm[msgs.Authenticate, msgs.Menu, Unit] =
      Sm.liftF(Authenticate(card, pin))
    val checkBalance: AtmSm[msgs.Menu, msgs.Authenticate, Int] =
      Sm.liftF(CheckBalance)
    val quit: AtmSm[msgs.Menu, msgs.Authenticate, Unit] =
      Sm.liftF(Quit)

    def interp(entry: ActRef[msgs.Authenticate]): Sm.Execute.Aux[AtmOp, IO, ActRef, msgs.Authenticate, msgs.Authenticate] = new Sm.Execute[AtmOp] {
      override type M[a] = IO[a]
      override type Res[st] = ActRef[st]
      override type InitSt = msgs.Authenticate
      override type FinSt = msgs.Authenticate
      override def init = IO.pure(entry)
      override def exec[F, T, A](res: ActRef[F])(d: AtmOp[F, T, A]): IO[(ActRef[T], A)] = d match {
        case Authenticate(c, p) =>
          for {
            resp <- res ? msgs.Authenticate(c, p)
            res <- resp match {
              case msgs.Failure() => IO.raiseError(new Exception("authentication error!!!"))
              case s @ msgs.Success() => IO.pure((ActRef(s.cont)(res.scheduler), ()))
            }
          } yield res
        case CheckBalance =>
          for {
            balance <- res ? msgs.CheckBalance()
          } yield (entry, balance.amount) // FIXME
        case Quit =>
          IO {
            res ! msgs.Quit()
            (entry, ())
          }
      }
      override def fin(ref: Res[msgs.Authenticate]): M[Unit] = IO.unit
    }
  }

  import client._

  val prog: client.AtmSm[msgs.Authenticate, msgs.Authenticate, Int] = for {
    _ <- authenticate("11232432", "1234")
    b <- checkBalance
  } yield b

  val mock1: Behavior[Any] = Actor.immutable { (ctx, msg) =>
    msg match {
      case a @ msgs.Authenticate(c, p) =>
        println("Successful authentication")
        a.cont ! msgs.Success()(ctx.self)
        Actor.same
      case m: msgs.Menu =>
        m match {
          case c @ msgs.CheckBalance() =>
            println("Returning balance")
            c.cont ! msgs.Balance(99)
          case msgs.Quit() =>
            println("Quit")
            ()
        }
        Actor.same
      case x =>
        println(s"unhandled: $x")
        Actor.same
    }
  }

  def test(): Int = {
    val sys = ActorSystem(mock1, "mock")
    val entry = ActRef[msgs.Authenticate](sys)(sys.scheduler)
    implicit val i: Sm.Execute.Aux[AtmOp, IO, ActRef, msgs.Authenticate, msgs.Authenticate] = interp(entry)
    try {
      prog.run[IO, ActRef].unsafeRunSync()
    } finally {
      sys.terminate()
    }
  }

  println(test())
}

/** From http://alcestes.github.io/lchannels/ */
object atm extends App {

  sealed trait NotAuthenticated
  sealed trait Authenticated

  sealed trait AtmOp[F, T, A]
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

  implicit val interp: Sm.Execute.Aux[AtmOp, IO, Const[Unit]#λ, NotAuthenticated, NotAuthenticated] = new Sm.Execute[AtmOp] {
    override type M[a] = IO[a]
    override type Res[st] = Unit
    override type InitSt = NotAuthenticated
    override type FinSt = NotAuthenticated
    override def init: IO[Unit] = IO.unit
    override def exec[F, T, A](res: Unit)(d: AtmOp[F, T, A]): IO[(Unit, A)] = d match {
      case Authenticate(c, p) =>
        if (p == "1234") {
          IO((res, println(s"Card ${c} accepted")))
        } else {
          IO.raiseError(new Exception("authentication error!!!"))
        }
      case CheckBalance =>
        IO {
          println("Returning balance")
          (res, 56)
        }
      case Quit => IO((res, println("Goodbye")))
    }
    override def fin(ref: Res[NotAuthenticated]): IO[Unit] = IO.unit
  }

  val goodPin: AtmSm[NotAuthenticated, NotAuthenticated, Int] = for {
    _ <- authenticate("11232432", "1234")
    b <- checkBalance
  } yield b

  val badPin: AtmSm[NotAuthenticated, NotAuthenticated, Int] = for {
    _ <- authenticate("11232432", "9876")
    b <- checkBalance
  } yield b

  val tsk1: IO[Int] = goodPin.run[IO, Const[Unit]#λ]
  val tsk2: IO[Int] = badPin.run[IO, Const[Unit]#λ]

  println(tsk1.unsafeRunSync())
  println(tsk2.unsafeRunSync())
}

/** From Idris States */
object varRef extends App {

  sealed trait VarOp[F, T, A]
  final case class Get[A]() extends VarOp[A, A, A]
  final case class Put[F, T](t: T) extends VarOp[F, T, Unit]

  type VarSm[F, T, A] = Sm[VarOp, F, T, A]

  def get[A]: VarSm[A, A, A] = Sm.liftF(Get())
  def put[F, T](t: T): VarSm[F, T, Unit] = Sm.liftF(Put(t))

  implicit def interp[T]: Sm.Execute.Aux[VarOp, IO, cats.Id, Unit, T] = new Sm.Execute[VarOp] {
    override type M[a] = IO[a]
    override type Res[st] = st
    override type InitSt = Unit
    override type FinSt = T
    override def init = IO.unit
    override def exec[F, T1, A](res: F)(d: VarOp[F, T1, A]): IO[(T1, A)] = d match {
      case g: Get[a] => IO.pure((res, res))
      case p: Put[f, t] => IO.pure((p.t, ()))
    }
    override def fin(ref: Res[T]) = IO.unit
  }

  val prog: VarSm[Unit, Double, Double] = for {
    _ <- Sm.pure[VarOp, Unit, Unit](())
    _ <- put(56)
    _ <- put(true)
    b <- get
    _ <- put(56.5)
    f <- get
  } yield if (b) f else 4.5

  println(prog.run[IO, cats.Id].unsafeRunSync())
}
