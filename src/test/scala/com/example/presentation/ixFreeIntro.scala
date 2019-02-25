/*
 * Copyright 2019 Daniel Urban and contributors listed in AUTHORS
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
package presentation

object FreeIntro {

  // (Simple) Free monad:
  trait Free[S[_], A] {
    def flatMap[B](f: A ⇒ Free[S, B]): Free[S, B]
  }
}


object IxFreeIntro {

  // Indexed Free monad:
  trait IxFree[S[_, _, _], F, T, A] {
    def flatMap[B, U](f: A ⇒ IxFree[S, T, U, B]):
      IxFree[S, F, U, B]
  }
}
