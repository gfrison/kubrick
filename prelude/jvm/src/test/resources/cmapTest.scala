/*
 * Copyright (c) 2025 Giancarlo Frison
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package kubrick.prelude

import cats.*
import cats.syntax.all.*
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

import cmap.{*, given}
class cmapTest extends AsyncFreeSpec with Matchers:
  "empty" in:
    Cmap.empty[Int, String] shouldBe Cmap.empty[Int, String]
  "new" in:
    Cmap(1 -> "L1", 2 -> "two") shouldBe Cmap(1 -> "L1", 2 -> "two")
  "append" in:
    (Cmap(1 -> "L1") |+| Cmap(2 -> "two")) shouldBe Cmap(1 -> "L1", 2 -> "two")
  "size 2" in:
    Cmap(1 -> "L1", 2 -> "two").size shouldBe 2
  "size 1" in:
    Cmap(1 -> "L1").size shouldBe 1
  "size 0" in:
    Cmap.empty[Int, String].size shouldBe 0
  "contains" in:
    Cmap(1 -> "L1", 2 -> "two").containsKey(1) shouldBe true
  "not contains" in:
    Cmap(1 -> "L1", 2 -> "two").containsKey(3) shouldBe false
  "bifunctor" in:
    val cmap   = Cmap(1 -> "one", 2 -> "two")
    val result = Bifunctor[Cmap].bimap(cmap)(_.toString, _.length)
    result shouldBe Cmap("1" -> 3, "2" -> 3)
  "from" in:
    val cmap = Cmap.from(Map(1 -> "one", 2 -> "two"))
    cmap shouldBe new Cmap(Map(1 -> "one", 2 -> "two"))
  "equality" in:
    new Cmap(Map("one" -> 1, "two" -> 2)) shouldBe new Cmap(List("one" -> 1, "two" -> 2))
    Cmap.from(Map("one" -> 1, "two" -> 2)) shouldBe new Cmap(List("one" -> 1, "two" -> 2))
