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

import scala.collection.immutable.ArraySeq

import builder.*
object all:
  case class Kid(value: Int) extends AnyVal
  type Iterm     = Term | Kid
  type Sequencer = () => Kid
  type Doc       = Lem[Term]
  type Bi        = M2m[Iterm, Kid]
  val bi0 = M2m.empty[Iterm, Kid]
  val K0  = Kube()
  class Nexter(seed: Int = 0) extends Sequencer:
    private val it   = Iterator.from(seed)
    def apply(): Kid = Kid(it.next())

  case class Kube(
      seqs: ArraySeq[Bi] = ArraySeq.empty,
      keys: Bi = bi0,
      vals: Bi = bi0,
      roots: Set[Kid] = Set.empty,
      sets: Set[Kid] = Set.empty
  ):
    infix def +(doc: Doc)(using next: Sequencer = Nexter(0)): Kube =
      (Builder(seqs, keys, vals, roots, sets) + doc) match
        case Builder(bSeqs, bKeys, bVals, bRoots, bSets) => Kube(bSeqs, bKeys, bVals, bRoots, bSets)

    def get(id: Kid): Option[Doc] = revit.Revit(seqs, keys, vals, sets).get(id)

    def search(query: Doc): LazyList[Doc] = ???
