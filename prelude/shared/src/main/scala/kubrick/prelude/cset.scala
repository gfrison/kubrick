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

import scala.collection.Searching
import scala.collection.immutable.IntMap
import scala.compiletime.asMatchable

import all.{*, given}
object cset:
  trait Cset[+T]:
    def contains[S >: T](elem: S): Boolean
    def +[U >: T](elem: U): Cset[U]
    def intersect[S >: T](other: Cset[S]): Cset[S]
    def diff[S >: T](other: Cset[S]): Cset[S]
    def ++[S >: T](other: Cset[S]): Cset[S]
    def headOption: Option[T]
    def tail: Cset[T]
  case object C0 extends Cset[Nothing]:
    override def headOption: Option[Nothing]  = None
    override def tail: Cset[Nothing]          = C0
    def ++[S](other: Cset[S]): Cset[S]        = other
    def intersect[S](other: Cset[S]): Cset[S] = C0
    def contains[S](elem: S): Boolean         = false
    def +[U](elem: U): Cset[U]                = Csort(IntMap(elem.hashCode -> IndexedSeq(elem)))
    def diff[S](other: Cset[S]): Cset[S]      = C0
  case class Csort[+T](hashes: IntMap[IndexedSeq[T]]) extends Cset[T]:
    override def headOption: Option[T] = hashes.values.headOption.flatMap(_.headOption)
    override def tail: Cset[T] = if hashes.isEmpty then C0
    else
      val newHashes = hashes.headOption
        .flatMap:
          case (h, vals) =>
            if vals.size == 1 then None
            else Some(h -> vals.tail)
        .map:
          case (h, vals) =>
            val rest = hashes.tail
            if rest.isEmpty then IntMap(h -> vals)
            else IntMap(h -> vals) ++ rest
        .getOrElse(hashes.tail)
      if newHashes.isEmpty then C0 else Csort[T](newHashes)
    def contains[S >: T](elem: S): Boolean =
      hashes
        .get(elem.hashCode)
        .map(exists(_, elem))
        .getOrElse(false)
    def +[U >: T](elem: U): Cset[U] =
      if contains(elem) then this
      else
        Csort(
          hashes.updated(
            elem.hashCode,
            sort((elem +: hashes.getOrElse(elem.hashCode, IndexedSeq.empty)).distinct)
          )
        )
    def intersect[S >: T](other: Cset[S]): Cset[S] = other match
      case C0 => C0
      case Csort(oHashes) =>
        val newHashes = hashes.flatMap:
          case (h, vals) =>
            oHashes.get(h) match
              case None => None
              case Some(oVals) =>
                val common = vals.filter(exists(oVals, _))
                if common.isEmpty then None else Some(h -> common)
        if newHashes.isEmpty then C0 else Csort(newHashes)
    def diff[S >: T](other: Cset[S]): Cset[S] =
      this.intersect(other) match
        case C0 => this
        case Csort(ohashes) =>
          val newHashes = hashes.flatMap:
            case (h, vals: IndexedSeq[S]) =>
              ohashes.get(h) match
                case None => Seq(h -> vals)
                case Some(oVals) =>
                  val res = vals.filterNot(exists(oVals, _))
                  if res.isEmpty then Seq.empty else Seq(h -> res)
          if newHashes.isEmpty then C0 else Csort(newHashes)
    def ++[S >: T](other: Cset[S]): Cset[S] = other match
      case C0 => this
      case Csort(oHashes) =>
        val newHashes = oHashes.foldLeft(hashes.asInstanceOf[IntMap[IndexedSeq[S]]]):
          case (acc, (h, oVals)) =>
            val combined = acc.get(h) match
              case None       => oVals
              case Some(vals) => sort((vals ++ oVals).distinct)
            acc.updated(h, combined)
        Csort(newHashes)

  private def exists[T](seq: IndexedSeq[T], elem: T): Boolean = elem.asMatchable match
    case atom: Term =>
      val iseq = seq.asInstanceOf[IndexedSeq[Term]] // unavoidable cast because of erasure
      iseq.search(atom) match
        case Searching.Found(_)          => true
        case Searching.InsertionPoint(_) => false
    case _ => seq.contains(elem)

  private def sort[T](seq: IndexedSeq[T]): IndexedSeq[T] = if seq.isEmpty then seq
  else
    seq.head.asMatchable match
      case _: Term =>
        val iseq = seq.asInstanceOf[IndexedSeq[Term]] // unavoidable cast because of erasure
        iseq.sorted.asInstanceOf[IndexedSeq[T]]
      case _ => seq
  object Cset:
    def empty[T]: Cset[T]                    = C0
    def from[T](elems: Iterable[T]): Cset[T] = Cset(elems.toSeq*)
    def apply[T](elems: T*): Cset[T] = elems.foldLeft(Cset.empty):
      case (acc, el) => acc + el
  given Functor[Cset]:
    def map[A, B](fa: Cset[A])(f: A => B): Cset[B] = fa match
      case C0            => C0
      case Csort(hashes) => Cset.from(hashes.values.flatten.map(f))
  given Traverse[Cset]:
    override def foldLeft[A, B](fa: Cset[A], b: B)(f: (B, A) => B): B = fa match
      case C0            => b
      case Csort(hashes) => hashes.values.flatten.foldLeft(b)(f)
    override def foldRight[A, B](fa: Cset[A], lb: Eval[B])(
        f: (A, Eval[B]) => Eval[B]
    ): Eval[B] = fa match
      case C0            => lb
      case Csort(hashes) => hashes.values.flatten.foldRight(lb)(f)
    def traverse[F[_]: Applicative, A, B](fa: Cset[A])(f: A => F[B]): F[Cset[B]] = fa match
      case C0            => Applicative[F].pure(C0)
      case Csort(hashes) => hashes.values.flatten.toSeq.traverse(f).map(bs => Cset.from(bs))
  given Foldable[Cset]:
    override def foldLeft[A, B](fa: Cset[A], b: B)(f: (B, A) => B): B =
      Traverse[Cset].foldLeft(fa, b)(f)
    override def foldRight[A, B](fa: Cset[A], lb: Eval[B])(
        f: (A, Eval[B]) => Eval[B]
    ): Eval[B] =
      Traverse[Cset].foldRight(fa, lb)(f)
  given SemigroupK[Cset]:
    def combineK[A](x: Cset[A], y: Cset[A]): Cset[A] = x ++ y
