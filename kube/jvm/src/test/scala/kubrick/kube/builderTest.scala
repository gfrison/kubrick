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

package kubrick.kube

import kubrick.kube.all.*
import kubrick.prelude.all.{*, given}
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import scribe.*
class builderTest extends AsyncFreeSpec with Matchers:
  Logger.root.withMinimumLevel(Level.Debug).replace()
  "l1" in:
    val kube = K0 + L1("a")
    kube.seqs.size shouldBe 1
    kube.seqs(0).getRight("a") should not be empty
  "sek1 (a,b,c)" in:
    val sek  = Sek("a", "b", "c")
    val kube = K0 + Sek("a", "b", "c")
    kube.seqs should have size 3
    kube.seqs(0).containsLeft("a") shouldBe true
    kube.roots should have size 1
    kube.roots.head shouldBe kube.seqs(0).getRight("a").head
    kube.roots.head shouldBe kube.seqs(1).getRight("b").head
  "sek nested" in:
    val kube = K0 + Sek(L1("a"), Sek(L1("b"), L1("c")), L1("d"))
    kube.seqs should have size 3
    kube.seqs(0).containsLeft("a") shouldBe true
    kube.seqs(0).containsLeft("b") shouldBe true
    kube.seqs(1).containsLeft("c") shouldBe true
    kube.seqs(2).containsLeft("d") shouldBe true
    kube.roots should have size 1
    kube.seqs(1).getLeft(kube.roots.head) should not be empty
  "seq + set" in:
    val kube = K0 + (Sek(L1("a"), Sek("b", "c")) + "d")
    kube.seqs should have size 2
    kube.sets should have size 1
    kube.keys.left.size shouldBe 1
    kube.roots should contain(kube.keys.getRight("d").head)
  "sek + choice" in:
    val kube = K0 + (Sek("a", "c") + Choice("d", "f"))
    kube.seqs should have size 2
    kube.roots should have size 1
    kube.sets shouldBe empty
    kube.keys.left.size shouldBe 2
    kube.keys.left.keySet should contain only("d", "f")
    val idc = kube.keys.left.values.toSet
    idc should have size 1
    idc.head shouldBe kube.roots.head
  "seq + pair" in:
    val kube = K0 + (Sek(L1("a"), Sek("b", "c")) + ("d" --> "f"))
    kube.seqs should have size 2
    kube.sets should have size 1
    kube.keys.left.size shouldBe 2
    kube.vals.left should have size 1
    kube.vals.getRight("f") shouldBe kube.keys.getRight("d")
  "choice of l1" in:
    val kube = K0 + Choice("a", "b", "c")
    kube.roots should have size 1
    kube.sets should have size 0
    kube.keys.left should have size 3
  "choice of pairs" in:
    val kube = K0 + Choice("a" --> 1, "b" --> 2, "c" --> 3)
    kube.roots should have size 1
    kube.sets should have size 0
    kube.keys.left should have size 6
    kube.vals.left should have size 3
    val s = kube.keys
      .getLeft(kube.roots.head)
      .flatMap:
        case id: Kid => kube.keys.getLeft(id)
    s shouldBe Set("a", "b", "c")
  "choice of seks" in:
    val kube = K0 + Choice(Sek("a", "b"), Sek("c", "d"))
    kube.sets should have size 0
    kube.roots should have size 1
    kube.keys.left should have size 2
    kube.seqs should have size 2
  "choice mix" in:
    val kube = K0 + Choice(L1("x"), "a" --> 1, Sek("b", "c"))
    kube.sets should have size 0
    kube.roots should have size 1
    kube.keys.left should have size 4
    kube.vals.left should have size 1
    kube.seqs should have size 2
  "pair1" in:
    val kube = K0 + ("a" --> "b")
    kube.keys.left should have size 2
    kube.vals.left should have size 1
    kube.roots should have size 1
  "pair2 l1 -> sek" in:
    val kube = K0 + (L1("a") --> Sek("b", "c"))
    kube.sets should have size 0
    kube.keys.left should have size 2
    kube.vals.left should have size 1
    kube.seqs should have size 2
    kube.roots should have size 1

  "sek choice" in:
    val d    = Sek("a", "b") + Choice("c", "d")
    val kube = K0 + d
    kube.sets shouldBe empty
