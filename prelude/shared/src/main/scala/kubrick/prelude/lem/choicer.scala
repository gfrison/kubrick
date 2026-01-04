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
import kubrick.prelude.lem.core.L0

import core.*
object choicer:
  extension [T, S[*] <: Lem[*]](c: S[T])(using cho: choicer[T, S]) infix def ||(lem: Lem[T]): S[T] = cho.or(c, lem)
  trait choicer[T, S[_]]:
    def or(a: S[T], other: Lem[T]): S[T]
  given ch: [T] => choicer[T, Choice]:
    def or(choice: Choice[T], other: Lem[T]): Choice[T] = other match
      case c: Choice[T] => Choice[T](choice.values ++ c.values)
      case _            => Choice[T](choice.values + other)
  given [T] => choicer[T, Lem]:
    def or(lem: Lem[T], that: Lem[T]): Lem[T] = (that, lem) match
      case _ -> L0             => that
      case (c: Choice[T]) -> _ => ch.or(c, lem)
      case _ -> _              => Choice[T](lem, that)
