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
