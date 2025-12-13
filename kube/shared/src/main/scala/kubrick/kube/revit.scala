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
import kubrick.prelude.all.*

import scala.collection.immutable.*

import all.*

object revit:
  case class Revit(
      seqs: ArraySeq[Bi],
      keys: Bi = bi0,
      vals: Bi = bi0
  ):
    def get(id: Kid): Lem[Term] =
      val lseq = seqs.foldLeft(ArraySeq.empty[Doc]):
        case (acc, bi) =>
          bi.getLeft(id) match
            case set if set.isEmpty => acc
            case set if set.size == 1 =>
              set.head match
                case t: Term => acc :+ L1(t)
                case k: Kid  => acc :+ get(k)
      (lseq, keys.getLeft(id)) match
        case ArraySeq() -> set if set.isEmpty             => L0
        case ArraySeq() -> set                            => L0
        case ArraySeq(l1: L1[Term]) -> set if set.isEmpty => l1
        case _ -> set =>
          val els = set.map:
            case k: Kid  => get(k)
            case t: Term => L1(t)
          Sek[Term](lseq, els)
