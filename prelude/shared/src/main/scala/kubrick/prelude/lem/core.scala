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

package kubrick.prelude.lem
import kubrick.prelude.cmap.*
import scribe.*
import scala.collection.immutable.ArraySeq
object core:
  sealed trait Lem[+T]:
    infix def -->[U >: T](that: Lem[U]): Pair[U] = Pair(this, that)

  case object L0 extends Lem[Nothing]

  case class L1[+T](value: T) extends Lem[T]

  case class Pair[+T](left: Lem[T], right: Lem[T]) extends Lem[T]

  case class Sek[+T](line: ArraySeq[Lem[T]], dict: Cmap[Lem[T], Lem[T]] = Cmap.empty) extends Lem[T]:
    export dict.{get, containsKey}

  object Sek:
    def apply[T](fst: T, snd: T, others: T*): Sek[T] = apply(ArraySeq(L1(fst), L1(snd)), Nil, others.toList.map(L1(_)))
    def apply[T](fst: Lem[T], snd: Lem[T], others: Lem[T]*): Sek[T] =
      apply(ArraySeq.empty[Lem[T]], Nil, fst :: snd :: others.toList)
    def apply[T](seq: ArraySeq[Lem[T]], dict: List[(Lem[T], Lem[T])], others: List[Lem[T]]): Sek[T] =
      others.toList match
        case Nil             => new Sek(seq, Cmap.from(dict.toMap))
        case Pair(l, r) :: t => apply(seq, (l, r) :: dict, t)
        case L0 :: t         => apply(seq, dict, t)
        case h :: t          => apply(seq :+ h, dict, t)

  case class Choice[+T](values: Cmap[Lem[T], L0.type]) extends Lem[T]
  object Choice:
    def apply[T](fst: T, snd: T, others: T*): Choice[T] = others.toList match
      case Nil    => new Choice(Cmap((L1(fst), L0), (L1(snd), L0)))
      case h :: t => apply(Cmap((L1(fst), L0), (L1(snd), L0), (L1(h), L0)), t.map(L1(_)))
    def apply[T](seq: Cmap[Lem[T], L0.type], others: List[Lem[T]]): Choice[T] = others.toList match
      case Nil    => new Choice(seq)
      case h :: t => apply(seq + (h, L0), t)
    def apply[T](fst: Lem[T], snd: Lem[T], others: List[Lem[T]]): Choice[T] = others.toList match
      case Nil    => new Choice(Cmap((fst, L0), (snd, L0)))
      case h :: t => apply(Cmap((fst, L0), (snd, L0), (h, L0)), t)
    def apply[T](fst: Lem[T], snd: Lem[T], others: Lem[T]*): Choice[T] = others.toList match
      case Nil    => new Choice(Cmap((fst, L0), (snd, L0)))
      case h :: t => apply(Cmap((fst, L0), (snd, L0), (h, L0)), t.toList)

  object --> {
    def unapply[T](lem: Lem[T]): Option[(Lem[T], Lem[T])] = lem match
      case pair: Pair[T] => Some((pair.left, pair.right))
      case _             => None
  }

  object +: {
    def unapply[T, S <: Lem[T]](sek: Sek[T]): Option[(Lem[T], Lem[T])] =
      sek.line.headOption.map: h =>
        h -> (sek.line.tail match
          case ArraySeq() if sek.dict.isEmpty => L0
          case ArraySeq()                     => new Sek(ArraySeq.empty, sek.dict)
          case ArraySeq(el)                   => new Sek(ArraySeq(el), sek.dict)
          case _                              => new Sek(sek.line.tail, sek.dict))

  }
  object :+ {
    def unapply[T, S <: Lem[T]](sek: Sek[T]): Option[(Lem[T], Lem[T])] =
      sek.line.lastOption.map: h =>
        h -> (sek.line.init match
          case ArraySeq() if sek.dict.isEmpty => L0
          case ArraySeq()                     => new Sek(ArraySeq.empty, sek.dict)
          case ArraySeq(el)                   => new Sek(ArraySeq(el), sek.dict)
          case _                              => Sek(sek.line.init, sek.dict))
  }
  object + {
    def unapply[T](lem: Lem[T]): Option[(Lem[T], Lem[T])] = lem match
      case Choice(values) =>
        debug(s"unappling choice:$lem")
        values.headOption.map:
          case (k: Lem[T], _) =>
            if values.tail.nonEmpty then (k, new Choice(values.tail))
            else (k, L0)
      case Sek[T](line, cmap) if cmap.nonEmpty =>
        cmap.headOption.map:
          case (k: Lem[T], L0) => k          -> sekTail(line, cmap.tail)
          case (k, v)          => Pair(k, v) -> sekTail(line, cmap.tail)
      case _ => None

    private def sekTail[T](line: ArraySeq[Lem[T]], cmap: Cmap[Lem[T], Lem[T]]): Lem[T] = (line.size, cmap.size) match
      case (0, 0) => L0
      case _ => Sek(line, cmap)

  }
