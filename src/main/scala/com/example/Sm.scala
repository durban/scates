package com.example

final class Sm[S[_, _, _], F, T, A] private (
  private val repr: IxFree[S, F, T, A]
) {

  import Sm.{ Initial, Final, Execute }

  def flatMap[B, U](f: A => Sm[S, T, U, B]): Sm[S, F, U, B] =
    new Sm(repr.flatMap(a => f(a).repr))

  def map[B](f: A => B): Sm[S, F, T, B] =
    new Sm(repr.map(f))

  def run[M[_] : cats.Monad : scalaz.Monad](implicit exec: Execute[S, M], F: Initial[S, F], T: Final[S, T]): M[A] = {
    val fx = new FunctionX[S, Sm.ResRepr[M, exec.Res]#λ] {
      def apply[G, U, X](sa: S[G, U, X]): scalaz.IndexedStateT[M, exec.Res[G], exec.Res[U], X] = {
        scalaz.IndexedStateT { res: exec.Res[G] =>
          exec.exec(res)(sa)
        }
      }
    }
    val st = repr.foldMap[Sm.ResRepr[M, exec.Res]#λ](fx)
    val initRes: exec.Res[F] = exec.init[F]
    val res: M[A] = cats.Monad[M].map(st.run(initRes))(_._2)
    res
  }
}

object Sm {

  type ResRepr[M[_], Res[_]] = {
    type λ[f, t, x] = scalaz.IndexedStateT[M, Res[f], Res[t], x]
  }

  trait Initial[S[_, _, _], A] // TODO: move A to type member

  trait Final[S[_, _, _], A] // TODO: move A to type member

  trait Execute[S[_, _, _], M[_]] {
    type Res[st]
    def init[F](implicit F: Sm.Initial[S, F]): Res[F]
    def exec[F, T, A](res: Res[F])(sa: S[F, T, A]): M[(Res[T], A)]
  }

  def pure[S[_, _, _], F, A](a: A)(implicit F: Initial[S, F]): Sm[S, F, F, A] =
    new Sm(IxFree.pure(a))

  def liftF[S[_, _, _], F, T, A](sa: S[F, T, A]): Sm[S, F, T, A] =
    new Sm(IxFree.liftF(sa))
}
