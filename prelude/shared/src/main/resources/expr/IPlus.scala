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

object IPlus:

  given [T] => IPlus[Sek, Pair]:
    type Out[X] = Sek[X]
    def plus[T](a: Sek[T], b: Pair[T]): Sek[T] = a.copy(pairs = a.pairs + (b.key, b.value))

  given [T] => IPlus[Sek, L1]:
    type Out[X] = Bag[X]
    def plus[T](a: Sek[T], b: L1[T]): Bag[T] = Bag(a, b)

  given [T] => IPlus[L1, NLem]:
    type Out[X] = Bag[X]
    def plus[T](a: L1[T], b: NLem[T]): Bag[T] = Bag(a, b)

  given [T] => IPlus[L1, L1]:
    type Out[X] = Bag[X]
    def plus[T](a: L1[T], b: L1[T]): Bag[T] = Bag(a, b)

  given [T] => IPlus[Bag, Pair]:
    type Out[X] = Bag[X]
    def plus[T](a: Bag[T], b: Pair[T]): Bag[T] = a.copy(pairs = a.pairs + (b.key, b.value))

  given [T] => IPlus[Bag, NLem]:
    type Out[X] = Bag[X]
    def plus[T](a: Bag[T], b: NLem[T]): Bag[T] = (a, b) match
      case (Bag(set1, pairs1), Bag(set2, pairs2)) => Bag(set1 ++ set2, pairs1 ++ pairs2)
      case (_, p: Pair[T])                        => a + p
      case (Bag(set, pairs), _)                   => Bag(set + b, pairs)

  given [T] => IPlus[NLem, NLem]:
    type Out[X] = Bag[X]
    def plus[T](a: NLem[T], b: NLem[T]): Bag[T] = (a, b) match
      case (bag: Bag[T], _) => bag + b
      case (_, bag: Bag[T]) => bag + a
      case _                => Bag(a, b)

  given [T] => IPlus[NLem, Lem]:
    type Out[X] = NLem[X]
    def plus[T](a: NLem[T], b: Lem[T]): NLem[T] = b match
      case L0         => a
      case x: NLem[T] => a + x
