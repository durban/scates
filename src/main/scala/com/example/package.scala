package com

import cats.effect.IO

package object example {

  type Const[C] = {
    type λ[x] = C
  }

  implicit def scalazMonadForCatsEffectIO: scalaz.Monad[IO] = new scalaz.Monad[IO] {
    override def bind[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa flatMap f
    override def point[A](a: => A): IO[A] = IO.pure(a)
  }
}
