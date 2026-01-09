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
import kubrick.prelude.lem.all.*

object Plus:
  /** add elements to an aggregate.
    *
    * specify argument type (I) and output type (Out) to resolve ambiguities.
    */
  trait Plus[F[_], I[_]]:
    type Out[_]
    def plus[T](a: F[T], b: I[T]): Out[T]

  extension [F[_], I[_], T](a: F[T])
    def +[O[_]](b: I[T])(using p: Plus[F, I] { type Out[X] = O[X] }): O[T] =
      p.plus(a, b)

  given [T] => Plus[Sek, Pair]:
    type Out[X] = Sek[X]
    def plus[T](a: Sek[T], b: Pair[T]): Sek[T] = a.copy(pairs = a.pairs + (b.left, b.right))

  given [T] => Plus[Sek, L1]:
    type Out[X] = Bag[X]
    def plus[T](a: Sek[T], b: L1[T]): Bag[T] = Bag(a, b)

  given [T] => Plus[L1, NLem]:
    type Out[X] = Bag[X]
    def plus[T](a: L1[T], b: NLem[T]): Bag[T] = Bag(a, b)

  given [T] => Plus[L1, L1]:
    type Out[X] = Bag[X]
    def plus[T](a: L1[T], b: L1[T]): Bag[T] = Bag(a, b)

  given [T] => Plus[Bag, Pair]:
    type Out[X] = Bag[X]
    def plus[T](a: Bag[T], b: Pair[T]): Bag[T] = a.copy(pairs = a.pairs + (b.left, b.right))

  given [T] => Plus[Bag, NLem]:
    type Out[X] = Bag[X]
    def plus[T](a: Bag[T], b: NLem[T]): Bag[T] = (a, b) match
      case (Bag(set1, pairs1), Bag(set2, pairs2)) => Bag(set1 ++ set2, pairs1 ++ pairs2)
      case (_, p: Pair[T])                        => summon[Plus[Bag, Pair] { type Out[X] = Bag[X] }].plus(a, p)
      case (Bag(set, pairs), _)                   => Bag(set + b, pairs)

  given [T] => Plus[NLem, NLem]:
    type Out[X] = Bag[X]
    def plus[T](a: NLem[T], b: NLem[T]): Bag[T] = (a, b) match
      case (bag: Bag[T], _) => summon[Plus[Bag, NLem] { type Out[X] = Bag[X] }].plus(bag, b)
      case (_, bag: Bag[T]) => summon[Plus[Bag, NLem] { type Out[X] = Bag[X] }].plus(bag, a)
      case _                => Bag(a, b)

  given [T] => Plus[NLem, Lem]:
    type Out[X] = NLem[X]
    def plus[T](a: NLem[T], b: Lem[T]): NLem[T] = b match
      case L0         => a
      case x: NLem[T] => summon[Plus[NLem, NLem] { type Out[X] = Bag[X] }].plus(a, x)
