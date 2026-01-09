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
import cats.*
import kubrick.prelude.all.*
import kubrick.prelude.cset.Cset

import core.*
object TraverseOps:
  given Traverse[Lem]:
    def foldLeft[A, B](fa: Lem[A], b: B)(f: (B, A) => B): B = fa match
      case x: NLem[A] => Traverse[NLem].foldLeft(x, b)(f)
      case L0         => b
    def foldRight[A, B](fa: Lem[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa match
      case x: NLem[A] => Traverse[NLem].foldRight(x, lb)(f)
      case L0         => lb
    def traverse[F[_]: Applicative, A, B](fa: Lem[A])(f: A => F[B]): F[Lem[B]] = fa match
      case x: NLem[A] => Traverse[NLem].traverse(x)(f).widen
      case L0         => Applicative[F].pure(L0)
  given Traverse[NLem]:
    def foldLeft[A, B](fa: NLem[A], b: B)(f: (B, A) => B): B = fa match
      case L1(value)    => f(b, value)
      case x: Pair[A]   => x.foldLeft(b)(f)
      case x: Sek[A]    => x.foldLeft(b)(f)
      case x: Choice[A] => x.foldLeft(b)(f)
      case x: Bag[A]    => x.foldLeft(b)(f)
    def foldRight[A, B](fa: NLem[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa match
      case L1(value)    => f(value, lb)
      case x: Pair[A]   => x.foldRight(lb)(f)
      case x: Sek[A]    => x.foldRight(lb)(f)
      case x: Choice[A] => x.foldRight(lb)(f)
      case x: Bag[A]    => x.foldRight(lb)(f)
    def traverse[F[_]: Applicative, A, B](fa: NLem[A])(f: A => F[B]): F[NLem[B]] = fa match
      case x: L1[A]     => f(x.value).map(L1(_))
      case x: Pair[A]   => x.traverse(f).widen
      case x: Sek[A]    => x.traverse(f).widen
      case x: Choice[A] => x.traverse(f).widen
      case x: Bag[A]    => x.traverse(f).widen

  given Traverse[L1]:
    override def foldLeft[A, B](fa: L1[A], b: B)(f: (B, A) => B): B                           = f(b, fa.value)
    override def foldRight[A, B](fa: L1[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = f(fa.value, lb)
    def traverse[F[_]: Applicative, A, B](fa: L1[A])(f: A => F[B]): F[L1[B]]                  = f(fa.value).map(L1(_))

  given Traverse[Pair]:
    override def foldLeft[A, B](fa: Pair[A], b: B)(f: (B, A) => B): B = fa.right.foldLeft(fa.left.foldLeft(b)(f))(f)
    override def foldRight[A, B](fa: Pair[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      fa.left.foldRight(fa.right.foldRight(lb)(f))(f)
    override def traverse[G[_]: Applicative, A, B](fa: Pair[A])(f: A => G[B]): G[Pair[B]] =
      Applicative[G].map2(fa.left.traverse(f), fa.right.traverse(f))(new Pair(_, _))

  given Traverse[Sek]:
    override def foldLeft[A, B](fa: Sek[A], b: B)(f: (B, A) => B): B =
      fa.line.foldLeft(fa.pairs.bifoldLeft(b)((acc, lem) => lem.foldLeft(acc)(f), (acc, lem) => lem.foldLeft(acc)(f)))(
        (acc, lem) => lem.foldLeft(acc)(f)
      )
    override def foldRight[A, B](fa: Sek[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      fa.line.foldRight(
        fa.pairs.bifoldRight(lb)((lem, acc) => lem.foldRight(acc)(f), (lem, acc) => lem.foldRight(acc)(f))
      )((lem, acc) => lem.foldRight(acc)(f))
    override def traverse[G[_]: Applicative, A, B](fa: Sek[A])(f: A => G[B]): G[Sek[B]] =
      Applicative[G].map2(
        fa.line.traverse(lem => Traverse[NLem].traverse(lem)(f)),
        fa.pairs.bitraverse(lem => Traverse[NLem].traverse(lem)(f), lem => Traverse[NLem].traverse(lem)(f))
      )(new Sek(_, _))

  given Traverse[Choice]:
    override def foldLeft[A, B](fa: Choice[A], b: B)(f: (B, A) => B): B = Traverse[Cset].foldLeft(fa.values, b):
      case (acc, lem) => lem.foldLeft(acc)(f)
    override def foldRight[A, B](fa: Choice[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      Traverse[Cset].foldRight(fa.values, lb):
        case (lem, acc) => lem.foldRight(acc)(f)
    override def traverse[G[_]: Applicative, A, B](fa: Choice[A])(f: A => G[B]): G[Choice[B]] =
      Traverse[Cset]
        .traverse(fa.values):
          case lem => Traverse[Lem].traverse(lem)(f)
        .map(new Choice(_))

  given Traverse[Bag]:
    override def foldLeft[A, B](fa: Bag[A], b: B)(f: (B, A) => B): B =
      fa.set.foldLeft(fa.pairs.bifoldLeft(b)((acc, lem) => lem.foldLeft(acc)(f), (acc, lem) => lem.foldLeft(acc)(f)))(
        (acc, lem) => lem.foldLeft(acc)(f)
      )
    override def foldRight[A, B](fa: Bag[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      fa.set.foldRight(
        fa.pairs.bifoldRight(lb)((lem, acc) => lem.foldRight(acc)(f), (lem, acc) => lem.foldRight(acc)(f))
      )((lem, acc) => lem.foldRight(acc)(f))
    override def traverse[G[_]: Applicative, A, B](fa: Bag[A])(f: A => G[B]): G[Bag[B]] =
      Applicative[G].map2(
        fa.set.traverse(lem => Traverse[NLem].traverse(lem)(f)),
        fa.pairs.bitraverse(lem => Traverse[NLem].traverse(lem)(f), lem => Traverse[NLem].traverse(lem)(f))
      )(new Bag(_, _))
