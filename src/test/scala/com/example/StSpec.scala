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

import shapeless.test.typed

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.Matchers
import org.scalatest.FlatSpec

class StSpec extends FlatSpec with Matchers with TypeCheckedTripleEquals {

  final class Door[S]

  /** -> Closed -> Open -> */
  object Door {

    def create =
      St.create[Door, Closed]

    implicit val init: St.Api.Initial[Door, Closed] =
      new St.Api.Initial[Door, Closed] {}
    implicit val fin: St.Api.Final[Door, Open] =
      new St.Api.Final[Door, Open] {}
    implicit val trOpen: St.Api.Transition[Door, Closed, Open] =
      new St.Api.Transition[Door, Closed, Open] {}
  }

  trait Closed
  trait Open

  type DoorLabel = St.Label { type Res[x] = Door[x] }

  val s1 = for {
    l1 <- St.create[Door, Closed]
    _ = typed[DoorLabel](l1)
    v1 <- St.pure(42)
    _ <- l1.delete
  } yield v1

  val s2 = for {
    l2 <- Door.create
    _ = typed[St.Label { type Res[x] = Door[x] }](l2)
    v2 <- St.pure("foo")
    _ <- l2.delete
  } yield v2

  val s3 = for {
    l1 <- St.create[Door, Closed]
    _ = typed[DoorLabel](l1)
    v1 <- St.pure(42)
    l2 <- Door.create
    _ = typed[DoorLabel](l2)
    _ <- l1.delete
    v2 <- St.pure("foo")
    _ <- l2.delete
  } yield v2

  val s4 = for {
    lb <- Door.create
    x <- lb.read[Closed] // this should be an error
    _ <- lb.write(new Door[Open])
    _ <- lb.delete
  } yield 42

  // TODO: okay, we have a problem here ...
  // What if we use the same creation
  // subprogram twice, like this:
  val mk = Door.create
  val s5 = for {
    l1 <- mk
    l2 <- mk
    // these probably will have the same label
    _ <- l1.delete
    // and this way we can leak `l2`
  } yield 42

  "Simple St" should "work" in {
    // TODO: this doesn't work:
    // s1.run.run
  }

  trait Var[A]

  object Var {

    def create =
      St.create[Var, Unit]

    implicit val init: St.Api.Initial[Var, Unit] =
      new St.Api.Initial[Var, Unit] {}
  }

  val v1 = for {
    l1 <- Var.create
    l2 <- Var.create
    v1 <- l1.read[Unit]
  } yield v1
}
