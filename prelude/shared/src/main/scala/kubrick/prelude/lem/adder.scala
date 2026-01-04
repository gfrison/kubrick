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

import scala.collection.immutable.ArraySeq
object adder:
  trait adder[T, W <: Lem[T], S <: Lem[T]]:
    extension (w: W) def +(lem: Lem[T]): S

  given [T] => adder[T, Sek[T], Sek[T]]:
    extension (sek: Sek[T])
      def +(lem: Lem[T]): Sek[T] = lem match
        case Pair(left, right) => sek.copy(dict = sek.dict + (left, right))
        case Sek(sline, sdict) => new Sek(sek.line ++ sline, sdict |+| sek.dict)
        case _                 => new Sek(sek.line, sek.dict + (lem, L0))
  given [T] => adder[T, Lem[T], Lem[T]]:
    extension (lem: Lem[T])
      def +(arg: Lem[T]): Lem[T] = (lem, arg) match
        case (Sek[T](l1, s1), Sek(l2, s2)) => Sek(l1 |+| l2, s1 |+| s2)
        case (Sek[T](line, set), _)        => Sek(line, set + (arg, L0))
        case (Choice(x), Choice(y))        => new Choice(x ++ y)
        case (_, Choice(y))                => new Choice(y + lem)
        case (L0, _)                       => arg
        case (_, L0)                       => lem
        case (_, _)                        => Sek.from(ArraySeq.empty, Set(lem, arg))
  given [T] => adder[T, Choice[T], Choice[T]]:
    extension (choice: Choice[T]) def +(lem: Lem[T]): Choice[T] = Choice[T](choice.values, lem :: Nil)
