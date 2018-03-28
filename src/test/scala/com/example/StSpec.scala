/*
 * Copyright 2018 Daniel Urban and contributors listed in AUTHORS
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

import scala.language.existentials

import cats.effect.IO

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.Matchers
import org.scalatest.FlatSpec

class StSpec extends FlatSpec with Matchers with TypeCheckedTripleEquals {

  val s1 = for {
    l1 <- St.create(42)
    v1 <- St.pure(42)
    l2 <- St.create("foo")
    _ <- l1.delete
    v2 <- St.pure("foo")
    _ <- l2.delete
  } yield v2

  "Simple St" should "work" in {
    val tsk: IO[String] = St.interpreter(s1)
    tsk.unsafeRunSync() should === ("foo")
  }
}
