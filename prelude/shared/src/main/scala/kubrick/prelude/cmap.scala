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

import scala.collection.immutable.IntMap
import scala.compiletime.asMatchable

object cmap:

  case class Cmap[+K, +V](entries: Iterable[(K, V)]):
    export entries.{size, isEmpty, nonEmpty}
    def tail: Cmap[K, V]           = Cmap(entries.tail)
    def head: (K, V)               = entries.head
    def headOption: Option[(K, V)] = entries.headOption
    private lazy val hashes = entries.foldLeft(IntMap.empty[Seq[(K, V)]]):
      case (acc, kvals) =>
        acc.updated(
          kvals._1.hashCode,
          acc.getOrElse(kvals._1.hashCode, Seq.empty) :+ kvals
        )
    def get[T >: K](key: T): Option[V] = hashes
      .get(key.hashCode)
      .flatMap: kvs =>
        kvs.find(_._1 == key).map(_._2)

    lazy val keys: Iterable[K]   = hashes.values.flatten.map(_._1)
    lazy val values: Iterable[V] = hashes.values.flatten.map(_._2)
    def containsKey[S >: K](key: S): Boolean = hashes
      .get(key.hashCode)
      .map(_.exists(_._1 == key))
      .getOrElse(false)

    def +[KK >: K, VV >: V](pair: (KK, VV)): Cmap[KK, VV] =
      if hashes.contains(pair._1.hashCode) && hashes.get(pair._1.hashCode).exists(_ == pair) then this
      else Cmap(entries ++ Seq(pair))

    override def hashCode(): Int = hashes.hashCode()
    override def equals(that: Any): Boolean = that.asMatchable match
      case that: Cmap[_, _] => that.hashes == this.hashes
      case _                => false

  object Cmap:

    def empty[K, V]: Cmap[K, V] = Cmap(Iterable.empty)

    def apply[K, V](entries: (K, V)*): Cmap[K, V] = new Cmap(entries)

    def from[K, V](map: Map[K, V]): Cmap[K, V] = new Cmap(map)

  given Bifunctor[Cmap]:
    def bimap[A, B, C, D](cmap: Cmap[A, B])(f: A => C, g: B => D): Cmap[C, D] =
      new Cmap(cmap.entries.map { case (k, v) => (f(k), g(v)) })

  given [K, V] => Semigroup[Cmap[K, V]]:
    def combine(x: Cmap[K, V], y: Cmap[K, V]): Cmap[K, V] = Cmap(x.entries ++ y.entries)

  given Bifoldable[Cmap]:
    def bifoldLeft[A, B, C](fab: Cmap[A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C =
      fab.entries.foldLeft(c):
        case (acc, (a, b)) => g(f(acc, a), b)

    def bifoldRight[A, B, C](fab: Cmap[A, B], c: Eval[C])(
        f: (A, Eval[C]) => Eval[C],
        g: (B, Eval[C]) => Eval[C]
    ): Eval[C] =
      fab.entries.foldRight(c):
        case ((a, b), acc) => g(b, f(a, acc))
  given Bitraverse[Cmap]:
    override def bifoldLeft[A, B, C](fab: Cmap[A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C = fab.bifoldLeft(c)(f, g)

    override def bifoldRight[A, B, C](fab: Cmap[A, B], c: Eval[C])(
        f: (A, Eval[C]) => Eval[C],
        g: (B, Eval[C]) => Eval[C]
    ): Eval[C] = fab.bifoldRight(c)(f, g)

    override def bitraverse[G[_]: Applicative, A, B, C, D](fab: Cmap[A, B])(f: A => G[C], g: B => G[D]): G[Cmap[C, D]] =
      fab.entries.toList
        .traverse:
          case (k, v) => (f(k), g(v)).mapN((ck, dv) => (ck, dv))
        .map(entries => Cmap(entries.toMap))

  given functorCmap: [K] => Functor[[V] =>> Cmap[K, V]]:
    def map[A, B](fa: Cmap[K, A])(f: A => B): Cmap[K, B] =
      Bifunctor[Cmap].bimap(fa)(identity, f)
