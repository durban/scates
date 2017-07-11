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

package com

import cats.effect.IO

package object example {

  type Const[C] = {
    type λ[x] = C
  }

  implicit def scalazMonadAndBindRecForCatsEffectIO: scalaz.Monad[IO] with scalaz.BindRec[IO] = {
    new scalaz.Monad[IO] with scalaz.BindRec[IO] {
      override def bind[A, B](fa: IO[A])(f: A => IO[B]): IO[B] =
        fa flatMap f
      override def point[A](a: => A): IO[A] =
        IO.pure(a)
      override def tailrecM[A, B](f: A => IO[scalaz.\/[A, B]])(a: A): IO[B] =
        IO.ioEffect.tailRecM(a) { a => f(a).map(_.toEither) }
    }
  }
}
