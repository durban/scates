/*
 * Copyright 2017-2018 Daniel Urban and contributors listed in AUTHORS
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

sealed trait HTree

sealed trait HNil extends HTree

sealed trait ::[H, T <: HTree] extends HTree {
  // FIXME: do we need these?
  type Head = H
  type Tail = T
}

sealed trait :+:[L <: HTree, R <: HTree] extends HTree {
  // FIXME: do we need these?
  type Left = L
  type Right = R
}

final object HTree {

  /** Only for debugging HTree problems */
  private[example] abstract class Debug[H <: HTree] {
    def debug: String
  }

  private[example] final object Debug extends Debug1 {

    def apply[A <: HTree](implicit inst: Debug[A]): Debug[A] =
      inst

    private[HTree] def instance[H <: HTree](s: String): Debug[H] = new Debug[H] {
      final override def debug: String = s
    }

    // TODO: also include `S`
    implicit def hconsWrite[S, L <: St.Label with Singleton, T <: HTree](
      implicit
      ttl: Trace[L],
      tail: Debug[T]
    ): Debug[St.Move[S, L] :: T] = Debug.instance(s"Move[${ttl.trace}] :: ${tail.debug}")
  }

  private[HTree] sealed abstract class Debug1 extends Debug2 { this: Debug.type =>
    // TODO: also include `S`
    implicit def hconsRead[S, L <: St.Label with Singleton, T <: HTree](
      implicit
      ttl: Trace[L],
      tail: Debug[T]
    ): Debug[St.InState[S, L] :: T] = Debug.instance(s"Read[${ttl.trace}] :: ${tail.debug}")
  }

  private[HTree] sealed abstract class Debug2 extends Debug3 { this: Debug.type =>
    implicit def fork[L <: HTree, R <: HTree](
      implicit
      left: Debug[L],
      right: Debug[R]
    ): Debug[L :+: R] = Debug.instance(s"(${left.debug}) :+: (${right.debug})")
  }

  private[HTree] sealed abstract class Debug3 { this: Debug.type =>
    implicit def hnil: Debug[HNil] =
      Debug.instance("HNil")
  }

  /** Only for tracing labels in `HTree.Debug` */
  private[example] trait Trace[A <: St.Label with Singleton] {
    def trace: String
  }

  private[example] final object Trace {

    import scala.language.experimental.macros

    def apply[A <: St.Label with Singleton](implicit inst: Trace[A]): Trace[A] =
      inst

    implicit def materialize[A <: St.Label with Singleton]: Trace[A] =
      macro TraceMacro.impl[A]
  }

  private[example] final object TraceMacro {

    import scala.reflect.macros.blackbox.Context

    final class TypeWrapper[U <: scala.reflect.macros.Universe with Singleton](private val tpe: U#Type) {

      override def equals(that: Any): Boolean = that match {
        case that: TypeWrapper[U] =>
          this.tpe =:= that.tpe
        case _ =>
          false
      }

      override def hashCode: Int =
        sys.error("a TypeWrapper is not hashable") // this should never be called
    }

    private[this] val memo: scala.collection.mutable.Map[TypeWrapper[_], Long] =
      new scala.collection.mutable.ListMap

    private[this] val cnt: java.util.concurrent.atomic.AtomicLong =
      new java.util.concurrent.atomic.AtomicLong(0L)

    def impl[A <: St.Label with Singleton](c: Context)(wt: c.WeakTypeTag[A]): c.Expr[Trace[A]] = {
      import c.universe._
      val wr = new TypeWrapper[c.universe.type](wt.tpe)
      val id = memo.synchronized {
        memo.get(wr) match {
          case None =>
            val id = cnt.getAndIncrement()
            memo += (wr -> id)
            id
          case Some(id) =>
            id
        }
      }
      c.Expr[Trace[A]](q"""new _root_.com.example.HTree.Trace[$wt] { override def trace: String = ${id.toString} }""")
    }
  }
}
