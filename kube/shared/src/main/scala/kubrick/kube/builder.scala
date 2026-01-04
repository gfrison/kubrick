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

import kubrick.prelude.all.{*, given}

import scala.collection.immutable.ArraySeq

import all.*
object builder:
  enum Side:
    case Key, Val, Root, Sets
  import Side.*
  case class Builder(
      seqs: ArraySeq[Bi],
      keys: Bi = bi0,
      vals: Bi = bi0,
      roots: Set[Kid],
      sets: Set[Kid]
  ):
    infix def +(doc: Doc)(using next: Sequencer): Builder =
      val id = next()
      doDoc(doc, id).add(id, Root)

    def doDoc(doc: Doc, id: Kid)(using next: Sequencer): Builder = doc match
      case L0             => this
      case L1(value)      => addPos(0, value, id)
      case Choice(alts)   => doChoice(alts, id)
      case p: Pair[Term]  => doSet(p, id)
      case sek: Sek[Term] => doSek(sek, id)

    def doSek(sek: Sek[Term], id: Kid)(using next: Sequencer): Builder = sek match
      case Sek(ArraySeq(), dict)           => doMap(dict, id)
      case Sek(line, dict) if dict.isEmpty => doLine(line, id)
      case Sek(line, dict)                 => doLine(line, id).doMap(dict, id)

    def doLine(line: ArraySeq[Doc], id: Kid)(using next: Sequencer): Builder =
      line.zipWithIndex.foldLeft(this):
        case (acc, (sek: Sek[Term], pos)) =>
          val nid = next()
          acc.doSek(sek, nid).addPos(pos, nid, id)
        case (acc, (L1(value), pos)) => acc.addPos(pos, value, id)

    def doMap(set: Cmap[Doc, Doc], parent: Kid)(using next: Sequencer): Builder =
      set.entries
        .foldLeft(this):
          case (acc, (Choice[Term](cset), L0)) => acc.doChoice(cset, parent)
          case (acc, (doc, L0))                => acc.doSet(doc, parent).add(parent, Sets)
          case (acc, (key, value))             => acc.doSet(Pair(key, value), parent).add(parent, Sets)

    def doChoice(alt: Cset[Doc], parent: Kid)(using next: Sequencer): Builder = alt.foldLeft(this):
      case (acc, doc) => acc.doSet(doc, parent)

    def doSet(doc: Doc, parent: Kid, side: Side = Key)(using next: Sequencer): Builder = doc match
      case L0           => this
      case L1(value)    => add(value, parent, side)
      case Choice(alts) => doChoice(alts, parent)
      case Pair(left, right) =>
        val keyId = next()
        doSet(left, keyId).add(keyId, parent, Key).doSet(right, keyId, Val)
      case sek: Sek[Term] =>
        val sid = next()
        doSek(sek, sid).add(sid, parent, side) // keys act as multiplexer for seks

    def add(value: Kid, kind: Root.type | Sets.type): Builder = kind match
      case Root => copy(roots = roots + value)
      case Sets => copy(sets = sets + value)
    def add(key: Iterm, value: Kid, kind: Side = Side.Key): Builder = kind match
      case Side.Key => copy(keys = keys + (key -> value))
      case Side.Val => copy(vals = vals + (key -> value))

    def addPos(pos: Int, value: Iterm, id: Kid): Builder =
      val nseqs = seqs.padTo(pos + 1, bi0)
      copy(seqs = nseqs.updated(pos, nseqs(pos) + (value, id)))
