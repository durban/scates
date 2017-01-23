package com.example

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
