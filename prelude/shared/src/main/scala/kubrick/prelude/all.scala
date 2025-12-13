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

object all:
  import cats.syntax.all.*
  import cats.*
  export lem.all.{*, given}
  export cmap.{*, given}
  export bimap.{*, given}
  export cset.{*, given}
  export m2m.{*, given}
  export urn.*
  export variable.*
  type Atom = Int | Float | String | Boolean
  type Term = Atom | Var
  given Ordering[Term]:
    def compare(x: Term, y: Term): Int = (x, y) match
      case (a: Int, b: Int)         => a.compareTo(b)
      case (a: Float, b: Float)     => a.compareTo(b)
      case (a: String, b: String)   => a.compareTo(b)
      case (a: Boolean, b: Boolean) => a.compareTo(b)
      case (a: Int, b: Float)       => a.toFloat.compareTo(b)
      case (a: Float, b: Int)       => a.compareTo(b.toFloat)
      case (_: Int, _: String)      => -1
      case (_: String, _: Int)      => 1
      case (_: Float, _: String)    => -1
      case (_: String, _: Float)    => 1
      case (_: Boolean, _: String)  => -1
      case (_: String, _: Boolean)  => 1
      case (_: Int, _: Boolean)     => -1
      case (_: Boolean, _: Int)     => 1
      case (_: Float, _: Boolean)   => -1
      case (_: Boolean, _: Float)   => 1
      case (x: Var, y: Var)         => x.name.compareTo(y.name)
      case (_: Atom, _: Var)        => -1
      case (_: Var, _: Atom)        => 1
