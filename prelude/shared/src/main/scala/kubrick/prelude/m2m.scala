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

import scala.collection.immutable.MultiDict
object m2m:
  case class M2m[L, R](left: MultiDict[L, R], right: MultiDict[R, L]):
    def containsLeft(l: L): Boolean  = left.containsKey(l)
    def containsRight(r: R): Boolean = right.containsKey(r)
    def getLeft(r: R): Set[L]        = right.get(r).toSet
    def getRight(l: L): Set[R]       = left.get(l).toSet
    def +(pair: (L, R)): M2m[L, R] = pair match
      case (l, r) => M2m(left + (l -> r), right + (r -> l))
    def ++(other: M2m[L, R]): M2m[L, R] = other match
      case M2m(a, b) => M2m(MultiDict.from(left.toSeq ++ a.toSeq), MultiDict.from(right.toSeq ++ b.toSeq))
  object M2m:
    def empty[L, R]: M2m[L, R] = M2m(MultiDict.empty, MultiDict.empty)
    def apply[L, R](pairs: (L, R)*): M2m[L, R] =
      pairs.foldLeft(empty[L, R]):
        case (acc, (l, r)) => acc + (l -> r)
  given Bifunctor[M2m]:
    def bimap[A, B, C, D](fab: M2m[A, B])(f: A => C, g: B => D): M2m[C, D] = fab match
      case M2m(left, right) => M2m(left.map((a, b) => f(a) -> g(b)), right.map((b, a) => (g(b), f(a))))
  given Bitraverse[M2m]:

    def bifoldLeft[A, B, C](fab: M2m[A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C = fab.left.toSeq.foldLeft(c):
      case (acc, (a, b)) => f(g(acc, b), a)

    def bifoldRight[A, B, C](fab: M2m[A, B], c: Eval[C])(
        f: (A, Eval[C]) => Eval[C],
        g: (B, Eval[C]) => Eval[C]
    ): Eval[C] =
      fab.left.toSeq.foldRight(c):
        case ((a, b), acc) => f(a, g(b, acc))

    def bitraverse[G[_]: Applicative, A, B, C, D](fab: M2m[A, B])(f: A => G[C], g: B => G[D]): G[M2m[C, D]] =
      val G                        = summon[Applicative[G]]
      val leftSeq: G[Seq[(C, D)]]  = fab.left.toSeq.traverse((a, b) => (f(a), g(b)).tupled)
      val rightSeq: G[Seq[(D, C)]] = fab.right.toSeq.traverse((b, a) => (g(b), f(a)).tupled)
      (leftSeq, rightSeq).mapN { (lseq, rseq) =>
        M2m(MultiDict.from(lseq), MultiDict.from(rseq))
      }
