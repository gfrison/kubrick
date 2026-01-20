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
import kubrick.prelude.all.*
import kubrick.prelude.cmap.given

import core.*
object cons:
  trait prepender[T, S[*] <: NLem[*]]:
    extension (w: NLem[T]) infix def +:(lem: S[T]): Sek[T]
  given [T] => prepender[T, Sek]:
    extension (lem: NLem[T])
      infix def +:(sek: Sek[T]): Sek[T] = lem match
        case L0                => sek
        case Pair(left, right) => sek.copy(pairs = sek.pairs + (left, right))
        case _                 => new Sek(lem +: sek.line, sek.pairs)
  given [T] => prepender[T, NLem]:
    extension (lem: NLem[T]) infix def +:(that: NLem[T]): Sek[T] = Sek(that, lem)
  trait appender[T, S[_]]:
    extension (w: S[T]) infix def :+(lem: NLem[T]): Sek[T]
  given [T] => appender[T, Sek]:
    extension (sek: Sek[T])
      infix def :+(lem: NLem[T]): Sek[T] = lem match
        case Pair(left, right) => sek.copy(pairs = sek.pairs + (left, right))
        case _                 => new Sek(sek.line :+ lem, sek.pairs)
  given [T] => appender[T, NLem]:
    extension (lem: NLem[T]) infix def :+(that: NLem[T]): Sek[T] = Sek(lem, that)
  trait merge[T, S <: Lem[T]]:
    extension (w: S) def ++(lem: S): S
  given [T] => merge[T, Choice[T]]:
    extension (choice: Choice[T]) def ++(lem: Choice[T]): Choice[T] = Choice[T](choice.values ++ lem.values, Nil)
  given [T] => merge[T, Sek[T]]:
    extension (lem: Sek[T]) def ++(that: Sek[T]): Sek[T] = new Sek(lem.line ++ that.line, lem.pairs |+| that.pairs)
