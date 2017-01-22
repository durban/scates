package com.example

import cats.Monad

import fs2.Task
import fs2.interop.cats._

sealed abstract class IxFree[S[_, _, _], F, T, A] extends Product with Serializable {

  import IxFree._

  def flatMap[B, U](f: A => IxFree[S, T, U, B]): IxFree[S, F, U, B] =
    FlatMapped(this, f)

  def map[B](f: A => B): IxFree[S, F, T, B] =
    flatMap(a => Pure(f(a)))

  final def step: IxFree[S, F, T, A] = this match {
    case FlatMapped(FlatMapped(c, f), g) =>
      c.flatMap(cc => f(cc).flatMap(g)).step
    case fm: FlatMapped[S, F, u, T, b, A] =>
      fm.s match {
        case p: Pure[S, f, A] =>
          fm.f(p.a).step
        case _ => fm
      }
    case x => x
  }

  final def foldMap[M[_, _, _]](f: FunctionX[S[?, ?, ?], M[?, ?, ?]])(implicit M: IxMonad[M]): M[F, T, A] = {
    def go[G, U, X](curr: IxFree[S, G, U, X]): M[G, U, X] = curr.step match {
      case p: Pure[S, G, X] =>
        M.pure[G, X](p.a)
      case s: Suspend[S, G, U, X] =>
        f(s.s)
      case fm: FlatMapped[S, G, u, U, y, X] =>
        val xxx = fm.s.foldMap(f)
        M.flatMap(xxx)(cc => go(fm.f(cc)))
    }
    go(this)
  }
}

object IxFree {

  // TODO: constraint
  def pure[S[_, _, _], F, A](a: A): IxFree[S, F, F, A] =
    Pure(a)

  def liftF[S[_, _, _], F, T, A](sa: S[F, T, A]): IxFree[S, F, T, A] =
    Suspend(sa)

  final case class Pure[S[_, _, _], F, A](a: A)
    extends IxFree[S, F, F, A]

  final case class Suspend[S[_, _, _], F, T, A](s: S[F, T, A])
    extends IxFree[S, F, T, A]

  final case class FlatMapped[S[_, _, _], F, T, U, A, B](
    s: IxFree[S, F, T, A], f: A => IxFree[S, T, U, B]
  ) extends IxFree[S, F, U, B]

  implicit def ixMonadInstance[S[_, _, _]]: IxMonad[IxFree[S, ?, ?, ?]] = new IxMonad[IxFree[S, ?, ?, ?]] {
    def flatMap[F, T, U, A, B](fa: IxFree[S, F, T, A])(f: A => IxFree[S, T, U, B]): IxFree[S, F, U, B] =
      fa flatMap f
    def pure[F, A](a: A): IxFree[S, F, F, A] =
      pure(a)
  }
}

final class Sm[S[_, _, _], F, T, A] private (private val repr: IxFree[S, F, T, A]) {

  import Sm.{ Initial, Final, Interpreter }

  def flatMap[B, U](f: A => Sm[S, T, U, B]): Sm[S, F, U, B] =
    new Sm(repr.flatMap(a => f(a).repr))

  def map[B](f: A => B): Sm[S, F, T, B] =
    new Sm(repr.map(f))

  def run[M[_] : Monad](i: Interpreter[S, M])(implicit F: Initial[S, F], T: Final[S, T]): M[A] = {
    val fx = new FunctionX[S, IxMonad.Fake[M]#λ] {
      def apply[G, U, X](sa: S[G, U, X]): M[X] =
        i.apply(sa)
    }
    repr.foldMap[IxMonad.Fake[M]#λ](fx)(IxMonad.ixMonadFromMonad[M])
  }
}

object Sm {

  trait Initial[S[_, _, _], A]

  trait Final[S[_, _, _], A]

  trait Interpreter[S[_, _, _], M[_]] {
    def apply[F, T, A](sa: S[F, T, A]): M[A]
  }

  def pure[S[_, _, _], F, A](a: A)(implicit F: Initial[S, F]): Sm[S, F, F, A] =
    new Sm(IxFree.pure(a))

  def liftF[S[_, _, _], F, T, A](sa: S[F, T, A]): Sm[S, F, T, A] =
    new Sm(IxFree.liftF(sa))
}

object example1 extends App {

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

object example2 extends App {

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

trait FunctionX[F[_, _, _], G[_, _, _]] {
  def apply[A, B, C](x: F[A, B, C]): G[A, B, C]
}

trait IxMonad[S[_, _, _]] {
  def flatMap[F, T, U, A, B](fa: S[F, T, A])(f: A => S[T, U, B]): S[F, U, B]
  def pure[F, A](a: A): S[F, F, A]
  def map[F, T, A, B](fa: S[F, T, A])(f: A => B): S[F, T, B] =
    flatMap(fa)(a => pure(f(a)))
}

object IxMonad {

  type Fake[S[_]] = {
    type λ[f, t, a] = S[a]
  }

  def monadFromIxMonad[S[_, _, _], F](implicit S: IxMonad[S]): Monad[S[F, F, ?]] = new Monad[S[F, F, ?]] {
    override def flatMap[A, B](sa: S[F, F, A])(f: A => S[F, F, B]): S[F, F, B] =
      S.flatMap(sa)(f)
    override def pure[A](a: A): S[F, F, A] =
      S.pure(a)
    override def tailRecM[A, B](a: A)(f: A => S[F, F, Either[A, B]]): S[F, F, B] =
      ??? // TODO
  }

  def ixMonadFromMonad[S[_]](implicit S: Monad[S]): IxMonad[Fake[S]#λ] = new IxMonad[Fake[S]#λ] {
    override def flatMap[F, T, U, A, B](fa: S[A])(f: A => S[B]): S[B] =
      S.flatMap(fa)(f)
    override def pure[F, A](a: A): S[A] =
      S.pure(a)
  }
}
