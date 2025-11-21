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
object variable:
  import VarXind.*
  trait Var extends AnyVar:
    val name: String
  object Lvar:
    def unapply(lvar: Lvar): Option[(String, VarXind)] = Some((lvar.name, lvar.kind))
  enum VarXind:
    def isNumeric: Boolean = this match
      case VarXind.int   => true
      case VarXind.float => true
      case VarXind.num   => true
      case _             => false
    case any, seq, mapset, string, int, float, urn, num, bool, primitive, expr
  trait AnyVar extends Any
  case object FreeVar extends Var:
    val name = "_"
    val kind = any
  case class Lvar(val name: String, val kind: VarXind = any) extends Var
  trait Vid extends AnyVar:
    val value: Int
  object Vid:
    def apply(value: Int): Vid         = Cid(value)
    def unapply(vid: Vid): Option[Int] = Some(vid.value)
  case class Cid(value: Int) extends Vid:
    override def toString(): String = s"v$value"
  case object Nid extends Vid:
    val value = -1
