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
import scribe.*

object revit:
  case class Revit(
      seqs: ArraySeq[Bi],
      keys: Bi = bi0,
      vals: Bi = bi0,
      sets: Set[Kid]
  ):
    def get(id: Kid): Option[Doc] =
      debug { s"get id=$id" }
      val (_, lseq) = seqs.foldLeft(true -> ArraySeq.empty[Doc]):
        case (true -> acc, bi) =>
          bi.getLeft(id) match
            case set if set.isEmpty => false -> ArraySeq.empty
            case set if set.size == 1 =>
              true -> {
                set.head match
                  case t: Term => acc :+ L1(t)
                  case k: Kid  => get(k).map(acc :+ _).getOrElse(acc)
              }
            case set => // choices
              val ms = set.map:
                case t: Term => L1(t)
                case k: Kid  => get(k).getOrElse(L0)
              if ms.tail.size == 1 then true -> (acc :+ Choice(ms.head, ms.tail.head))
              else true                      -> (acc :+ Choice(ms.head, ms.tail.head, ms.tail.tail.toSeq*))
        case _ => false -> ArraySeq.empty

      (lseq, keys.getLeft(id)) match
        case ArraySeq(l1: L1[Term]) -> set if set.isEmpty => Some(l1)
        case ArraySeq() -> set if set.isEmpty             => None
        case ArraySeq() -> set                            => fromKey(id, set)
        case _ -> set =>
          Some:
            val els = set
              .map:
                case k: Kid  => get(k)
                case t: Term => Some(L1(t))
              .flatten
            Sek.from[Term](lseq, fromKey(id, set).toSet)

    def fromKey(id: Kid, idKeys: Set[Term | Kid]): Option[Doc] =
      val idVals = fromValue(vals.getLeft(id))
      val isSet  = sets.contains(id)
      debug { s"fromKey isSet=$isSet idKeys=$idKeys idVals=$idVals" }
      val al = idKeys.map:
        case t: Term => L1(t)
        case k: Kid  => get(k).getOrElse(L0)
      if al.isEmpty then None
      else
        Some:
          val el = {
            if isSet then Sek.from(ArraySeq.empty, al)
            else if al.size == 1 then al.head
            else Choice.from(al.toList)
          }
          idVals match
            case Some(v) => Pair(el, v)
            case None    => el

    def fromValue(idVals: Set[Term | Kid]): Option[Doc] =
      val al = idVals.map:
        case t: Term => L1(t)
        case k: Kid  => get(k).getOrElse(L0)
      if al.isEmpty then None
      else
        Some:
          if al.size == 1 then al.head
          else Choice(al.head, al.tail.head, al.tail.tail.toSeq*)
