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

import cats.Eval
import kubrick.prelude.cmap.*
import kubrick.prelude.lem.*
import kubrick.prelude.lem.all.{*, given}
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.ArraySeq

class lem2Test extends AsyncFreeSpec with Matchers:
  "construction" in:
    Sek(1, 2, 3) shouldBe Sek(L1(1), L1(2), L1(3))
    1 +: Sek(2, 3) shouldBe Sek(1, 2, 3)
    1 ++: Sek(2, 3) shouldBe Sek(1, Sek(2, 3))
    1 ++: 2 shouldBe Sek(1, 2)
    Sek(1, 2) + 3 shouldBe new Sek(ArraySeq(L1(1), L1(2)), Cmap((L1(3), L0)))
    Sek(1, 2) + Sek(3, 4) shouldBe new Sek(ArraySeq(L1(1), L1(2), L1(3), L1(4)))
    1 ++ 2 shouldBe Choice(1, 2)
    1 --> 2 shouldBe Pair(L1(1), L1(2))
    (1 --> 2) ++: 3 shouldBe Sek(Pair(L1(1), L1(2)), L1(3))
    (((1 --> 2) :++ 3) + 4) shouldBe (Sek(Pair(1, 2), L1(3)) + 4)

  "map aggregate" in:
    L1(10).map(_ + 1) shouldBe L1(11)
    Pair(L1(1), L1(2)).map(_ + 1) shouldBe Pair(L1(2), L1(3))
    L0.map((_: Int) => 42) shouldBe L0

  "traverse aggregate" in:
    val l1 = L1(5)
    l1.traverse(x => Option(x * 2)) shouldBe Option(L1(10))
    l1.traverse(_ => None) shouldBe None

    val p = Pair(L1(1), L1(2))
    p.traverse(x => Option(x + 1)) shouldBe Option(Pair(L1(2), L1(3)))
    p.traverse(_ => None) shouldBe None

    val s = Sek(L1(1), L1(2))
    s.traverse(x => Option(x * 3)) shouldBe Option(Sek(L1(3), L1(6)))
    s.traverse(_ => None) shouldBe None

    val l: Lem[Int] = L1(7)
    l.traverse(x => Option(x * 2)) shouldBe Option(L1(14))
    l.traverse(_ => None) shouldBe None
    val p2: Lem[Int] = Pair(L1(2), L1(3))
    p2.traverse(x => Option(x * 2)) shouldBe Option(Pair(L1(4), L1(6)))
    p2.traverse(_ => None) shouldBe None

  "Sek.exists and containsKey" in:
    val sek = Sek(1, 2, 3) + 12
    sek.exists(_ == 12) shouldBe true
    sek.exists(_ == 42) shouldBe false
    val s2 = Sek(L1(1), L1(2)) + 3 + (100 --> 200)
    s2.containsKey(L1(3)) shouldBe true
    s2.containsKey(L1(99)) shouldBe false
    s2.get(L1(100)) shouldBe Some(L1(200))
    s2.containsKey(L1(100)) shouldBe true

  "Sek.foldLeft and foldRight" in:
    val sek = Sek(L1(1), L1(2), L1(3))
    sek.foldLeft(0)((acc, l) => acc + l) shouldBe 6
    sek.foldRight(Eval.now(0))((l, acc) => acc.map(a => l + a)).value shouldBe 6
    sek.foldLeft(1)((acc, l) => acc * l) shouldBe 6

  "unapply Sek" in:
    val sek = Sek(1, 2, 3)
    val (head, tail) = sek match
      case h +: t => (h, t)
    head shouldBe L1(1)
    tail shouldBe Sek(2, 3)

  "unapply -->" in:
    val pair = 1 --> 2
    val (left, right) = pair match
      case l --> r => (l, r)
    left shouldBe L1(1)
    right shouldBe L1(2)
