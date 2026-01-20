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

import cmap.{*, given}
object bimap:
  trait Bimap[+K, +V]:
    def getValue[T >: K](key: T): Option[V]
    def value[T >: K](key: T): V = getValue(key).get
    def getKey[T >: V](value: T): Option[K]
    def key[T >: V](value: T): K = getKey(value).get
    def +[T >: K, U >: V](entry: (T, U)): Bimap[T, U]
    def existsKey[T >: K](key: T): Boolean     = getValue(key).isDefined
    def existsValue[T >: V](value: T): Boolean = getKey(value).isDefined
    def putIfAbsent[T >: K, U >: V](key: T, value: () => U): Bimap[T, U] =
      if existsKey(key) then this
      else this + (key -> value())

  case object Bi0 extends Bimap[Nothing, Nothing]:
    def getValue[T](key: T): Option[Nothing] = None
    def getKey[T](value: T): Option[Nothing] = None
    def +[T, U](entry: (T, U)): Bimap[T, U]  = Bim() + entry

  case class Bim[K, V](few: Cmap[K, V] = Cmap.empty, rev: Cmap[V, K] = Cmap.empty) extends Bimap[K, V]:
    def getValue[T >: K](key: T): Option[V] = few.get(key)
    def getKey[T >: V](value: T): Option[K] = rev.get(value)
    def +[T >: K, U >: V](entry: (T, U)): Bimap[T, U] = entry match
      case (key, value) => Bim(few + (key -> value), rev + (value -> key))

  object Bimap:
    def empty[K, V]: Bimap[K, V]                           = Bi0
    def from[K, V](entries: Iterable[(K, V)]): Bimap[K, V] = entries.foldLeft(Bi0)(_ + _)
    def apply[K, V](entries: (K, V)*): Bimap[K, V]         = from(entries)

  given Bifunctor[Bimap]:
    def bimap[A, B, C, D](fab: Bimap[A, B])(f: A => C, g: B => D): Bimap[C, D] = fab match
      case Bi0           => Bi0
      case Bim(few, rev) => Bim(few.bimap(f, g), rev.bimap(g, f))

  given Bifoldable[Bimap]:
    def bifoldRight[A, B, C](fab: Bimap[A, B], c: Eval[C])(
        f: (A, Eval[C]) => Eval[C],
        g: (B, Eval[C]) => Eval[C]
    ): Eval[C] = fab match
      case Bi0           => c
      case Bim(few, rev) => rev.bifoldRight(c)(g, f)
    def bifoldLeft[A, B, C](fab: Bimap[A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C = fab match
      case Bi0           => c
      case Bim(few, rev) => few.bifoldLeft(c)(f, g)

  given [K] => Foldable[[x] =>> Bimap[K, x]]:
    override def foldLeft[A, B](fab: Bimap[K, A], b: B)(f: (B, A) => B): B =
      Bifoldable[Bimap].bifoldLeft[K, A, B](fab, b)((acc, _) => acc, f)
    override def foldRight[A, B](fab: Bimap[K, A], lb: Eval[B])(
        f: (A, Eval[B]) => Eval[B]
    ): Eval[B] =
      Bifoldable[Bimap].bifoldRight[K, A, B](fab, lb)((_, acc) => acc, f)

  given Bitraverse[Bimap]:
    override def bifoldLeft[A, B, C](fab: Bimap[A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C =
      Bifoldable[Bimap].bifoldLeft(fab, c)(f, g)
    override def bifoldRight[A, B, C](fab: Bimap[A, B], c: Eval[C])(
        f: (A, Eval[C]) => Eval[C],
        g: (B, Eval[C]) => Eval[C]
    ): Eval[C] =
      Bifoldable[Bimap].bifoldRight(fab, c)(f, g)
    def bitraverse[F[_]: Applicative, A, B, C, D](fab: Bimap[A, B])(
        f: A => F[C],
        g: B => F[D]
    ): F[Bimap[C, D]] = fab match
      case Bi0 => Applicative[F].pure(Bi0)
      case Bim(few, rev) =>
        val fcmap: F[Cmap[C, D]] = few.bitraverse(f, g)
        val rcmap: F[Cmap[D, C]] = rev.bitraverse(g, f)
        (fcmap, rcmap).mapN((fcm, rcm) => Bim(fcm, rcm))
