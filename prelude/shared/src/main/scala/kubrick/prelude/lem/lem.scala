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
import cats.syntax.all.*
import kubrick.prelude.cmap.{*, given}

import scala.collection.immutable.ArraySeq
import scala.util.NotGiven
object core:
  sealed trait Lem[+T]:
    infix def ++:[U >: T](that: Lem[U]): Sek[U]   = Sek(that, this)
    infix def :++[U >: T](that: Lem[U]): Sek[U]   = Sek(this, that)
    infix def ++[U >: T](that: Lem[U]): Choice[U] = Choice(this, that)
    infix def -->[U >: T](that: Lem[U]): Pair[U]  = Pair(this, that)

  case object L0 extends Lem[Nothing]

  case class L1[+T](value: T) extends Lem[T]

  case class Pair[+T](left: Lem[T], right: Lem[T]) extends Lem[T]

  case class Sek[+T](line: ArraySeq[Lem[T]], dict: Cmap[Lem[T], Lem[T]] = Cmap.empty) extends Lem[T]:
    def +:[U >: T](that: Pair[U]): Sek[U]                                       = this.+(that)
    def :+[U >: T](that: Pair[U]): Sek[U]                                       = this.+(that)
    def +:[U >: T, S <: Lem[U]](that: S)(using NotGiven[S =:= Pair[U]]): Sek[U] = new Sek(that +: line, dict)
    def :+[U >: T, S <: Lem[U]](that: S)(using NotGiven[S =:= Pair[U]]): Sek[U] = new Sek(line :+ that, dict)
    def +[U >: T](that: Pair[U]): Sek[U] = copy(dict = dict + (that.left, that.right))
    def +[U >: T, S <: Lem[U]](that: S)(using NotGiven[S =:= Pair[U]]): Sek[U] = that match
      case Sek(sline, sdict) => new Sek(line ++ sline, sdict |+| this.dict)
      case _                 => new Sek(line, dict + (that, L0))
    export dict.{get, containsKey}

  object Sek:
    def apply[T](fst: T, snd: T, others: T*): Sek[T] = others.toList match
      case Nil    => new Sek[T](ArraySeq(L1(fst), L1(snd)), Cmap.empty)
      case h :: t => apply(ArraySeq(L1(fst), L1(snd), L1(h)), t.map(L1(_)))
    def apply[T](fst: T, snd: Lem[T], others: T*): Sek[T] = others.toList match
      case Nil    => new Sek(ArraySeq(L1(fst), snd), Cmap.empty)
      case h :: t => apply(ArraySeq(L1(fst), snd, L1(h)), t.map(L1(_)))
    def apply[T](seq: ArraySeq[Lem[T]], others: List[Lem[T]]): Sek[T] = others.toList match
      case Nil    => new Sek(seq, Cmap.empty)
      case h :: t => apply(seq :+ h, t)
    def apply[T](fst: Lem[T], snd: Lem[T], others: Lem[T]*): Sek[T] = others.toList match
      case Nil    => new Sek(ArraySeq(fst, snd), Cmap.empty)
      case h :: t => apply(ArraySeq(fst, snd, h), t.toList)

  case class Choice[+T](values: Cmap[Lem[T], L0.type]) extends Lem[T]:
    def +[U >: T](that: Lem[U]): Lem[U] = Choice[U](values, that :: Nil)

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
    def unapply[T](pair: Pair[T]): Option[(Lem[T], Lem[T])] = Some((pair.left, pair.right))
  }

  object +: {
    def unapply[T, S <: Lem[T]](sek: Sek[T]): Option[(Lem[T], Sek[T])] =
      sek.line.headOption.map(h => (h, Sek(sek.line.tail, sek.dict)))
  }
