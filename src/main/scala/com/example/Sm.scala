package com.example

final class Sm[S[_, _, _], F, T, A] private (
  private val repr: IxFree[S, F, T, A]
) {

  import Sm.{ Execute, Create }

  def flatMap[B, U](f: A => Sm[S, T, U, B]): Sm[S, F, U, B] =
    new Sm(repr.flatMap(a => f(a).repr))

  def map[B](f: A => B): Sm[S, F, T, B] =
    new Sm(repr.map(f))

  // TODO: infer `R`
  def run[M[_] : cats.Monad : scalaz.Monad, R[_]](
    implicit
    exec: Execute.Aux[S, M, R],
    mk: Create.Aux[S, R, F],
  ): M[A] = {
    val fx = new FunctionX[S, Sm.ResRepr[M, exec.Res]#λ] {
      def apply[G, U, X](sa: S[G, U, X]): scalaz.IndexedStateT[M, exec.Res[G], exec.Res[U], X] = {
        scalaz.IndexedStateT { res: exec.Res[G] =>
          exec.exec(res)(sa)
        }
      }
    }
    val st = repr.foldMap[Sm.ResRepr[M, exec.Res]#λ](fx)
    val initRes: exec.Res[F] = mk.mk
    val res: M[A] = cats.Monad[M].map(st.run(initRes))(_._2)
    res
  }
}

object Sm {

  type ResRepr[M[_], Res[_]] = {
    type λ[f, t, x] = scalaz.IndexedStateT[M, Res[f], Res[t], x]
  }

  trait Initial[S[_, _, _]] {
    type F
  }

  object Initial {

    type Aux[S[_, _, _], A] = Initial[S] {
      type F = A
    }

    def define[S[_, _, _], A]: Initial.Aux[S, A] =
      new Initial[S] { type F = A }
  }

  trait Final[S[_, _, _], A]

  trait Create[S[_, _, _]] {
    type Res[st]
    type Init
    def mk: Res[Init]
  }

  object Create {
    
    type Aux[S[_, _, _], R[_], I] = Create[S] {
      type Res[st] = R[st]
      type Init = I
    }
    
    def apply[S[_, _, _]](implicit inst: Create[S]): Create.Aux[S, inst.Res, inst.Init] =
      inst
    
    def instance[S[_, _, _], R[_], I](create: => R[I]): Create.Aux[S, R, I] = new Create[S] {
      type Res[st] = R[st]
      type Init = I
      def mk: R[I] = create
    }
  }

  trait Execute[S[_, _, _], M[_]] {
    type Res[st]
    def exec[F, T, A](res: Res[F])(sa: S[F, T, A]): M[(Res[T], A)]
  }

  object Execute {
    type Aux[S[_, _, _], M[_], R[_]] = Execute[S, M] {
      type Res[st] = R[st]
    }
  }

  def pure[S[_, _, _], A](a: A)(implicit i: Initial[S]): Sm[S, i.F, i.F, A] =
    new Sm(IxFree.pure(a))

  def liftF[S[_, _, _], F, T, A](sa: S[F, T, A]): Sm[S, F, T, A] =
    new Sm(IxFree.liftF(sa))
}
