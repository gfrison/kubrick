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

import scala.collection.immutable.ArraySeq
object urn:
  trait Urn:
    type Kind <: Urn
    def parent: Option[Kind]
    def childOf(parent: Urn): Boolean
    def parentOf(child: Urn): Boolean
    def /(seg: String): Kind
    def /(seg: Int): Kind
  case class Ns(ns: String, segments: ArraySeq[String]) extends Urn:
    type Kind = Ns
    override def parent: Option[Ns] =
      if segments.isEmpty then None else Some(Ns(ns, segments.init))
    override def /(seg: Int): Ns               = copy(segments = segments :+ seg.toString)
    override def /(seg: String): Ns            = copy(segments = segments :+ seg)
    override def childOf(parent: Urn): Boolean = this.parent == parent
    override def parentOf(child: Urn): Boolean = child.parent == this
    override def toString(): String            = s"$ns:${segments.mkString("/")}"

  object Ns:
    def apply(ns: String, resources: String*): Ns = new Ns(ns, ArraySeq.from(resources))
  case class Uri(base: String, segments: ArraySeq[String]) extends Urn:
    type Kind = Uri
    override def parent: Option[Uri] =
      if segments.isEmpty then Some(new Uri(base, ArraySeq.empty)) else Some(Uri(base, segments.init))
    override def /(seg: Int): Uri              = copy(segments = segments :+ seg.toString)
    override def /(seg: String): Uri           = copy(segments = segments :+ seg)
    override def childOf(parent: Urn): Boolean = this.parent == parent
    override def parentOf(child: Urn): Boolean = child.parent == this
    override def toString(): String            = s"$base/${segments.mkString("/")}"
  object Uri:
    def apply(uri: String) =
      uri.indexOf("/") match
        case -1 => new Uri(uri, ArraySeq.empty)
        case i  => new Uri(uri.substring(0, i), ArraySeq.from(uri.substring(i + 1).split("/")))
