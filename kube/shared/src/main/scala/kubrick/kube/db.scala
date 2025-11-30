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

package kubrick.kube
import kubrick.prelude.all.*
import scribe.*

import scala.collection.immutable.ArraySeq
object db:
  case class Pos(value: Int)  extends AnyVal
  case class Key(value: Atom) extends AnyVal
  type ktype = Atom | Pos | Key
  type Path  = ArraySeq[ktype]
  type Doc   = Lem[Atom]
  type Store = Map[ArraySeq[ktype], Set[Int]]
  extension (self: (Path, Path))
    def **(that: (Path, Path)): (Path, Path) =
      (self._1 ++ that._1) -> (self._2 ++ that._2)

  extension (pp: LazyList[(Path, Path)])
    def **(that: LazyList[(Path, Path)]): LazyList[(Path, Path)] =
      for
        p1 <- pp
        p2 <- that
      yield p1 ** p2

  object DB:
    def apply(data: Doc*): DB = new DB(ArraySeq.from(data))
    def from(data: Iterable[Doc]): DB = data match
      case s: ArraySeq[Doc] => new DB(s.distinct)
      case s: Set[Doc]      => new DB(ArraySeq.from(s))
      case s: Seq[Doc]      => new DB(ArraySeq.from(s.distinct))

  class DB(data: ArraySeq[Doc]):
    lazy val index: Store = data.zipWithIndex.foldLeft(Map.empty[ArraySeq[ktype], Set[Int]]):
      case (acc, (doc, idx)) =>
        merge(acc, db(doc = doc, init = true).map((ands, ors) => ands ++ ors).map(path => path -> Set(idx)).toMap)

    def docSearch(query: Doc): LazyList[Doc] =
      debug { s"db: $index,\n data: $data" }
      val paths = db(doc = query, init = true)
        .map: path =>
          debug(s"search path:$path")
          path
      val orPaths = paths
        .map(_._2)
        .map(index.get)
        .flatten
        .reduceOption(_ ++ _)
      paths
        .map(_._1)
        .foldLeft(orPaths):
          case (Some(acc), path) if acc.nonEmpty =>
            index.get(path) match
              case Some(ids) => Some(acc intersect ids)
              case None      => Some(Set.empty)
          case (None, path) =>
            index.get(path) match
              case Some(ids) => Some(ids)
              case None      => Some(Set.empty)
          case _ => Some(Set.empty)
        .map: ids =>
          ids.to(LazyList).map(data)
        .getOrElse(LazyList.empty)

  enum Kind:
    case positionk, keyk
  enum Aggr:
    case and, or
  import Kind.*
  import Aggr.*
  private def apply(
      doc: Doc,
      pos: Int = 0,
      kind: Kind = positionk,
      init: Boolean = false,
      aggr: Aggr = and
  ): LazyList[(Path, Path)] =
    doc match
      case L0              => LazyList.empty
      case L1(value: Atom) => LazyList(build(kind, value, pos, aggr))
      case Pair(left, right) =>
        db(doc = left, kind = keyk).flatMap: head =>
          db(doc = right).map: tail =>
            head ** tail
      case sek: Sek[Atom] =>
        doSek(sek).map(_.map(path => if (path.nonEmpty && !init) then Pos(pos) +: path else path))
      case choice: Choice[Atom] => doChoice(choice)
  private def doChoice(choice: Choice[Atom]): LazyList[(Path, Path)] = choice match
    case h + (t: Choice[Atom]) => db(doc = h, kind = keyk, aggr = or) ++ doChoice(t)
    case h + L0 => db(doc = h, kind = keyk, aggr = or)
    case lem: Lem[Atom]        => db(doc = lem, kind = keyk, aggr = or)
  private def doSek(sek: Sek[Atom], pos: Int = 0): LazyList[(Path, Path)] = sek match
    case h +: (t: Sek[Atom]) => db(h, pos = pos) ++ doSek(t, pos = pos + 1)
    case h + (t: Sek[Atom])  => db(h) ++ doSek(t)
    case h +: t              => db(h, pos = pos) ++ db(t, pos = pos + 1)
    case h + t               => db(h) ++ db(t)

  private def build(kind: Kind, value: Atom, pos: Int, aggr: Aggr): (Path, Path) =
    val path = kind match
      case Kind.positionk => ArraySeq(Pos(pos), value)
      case Kind.keyk      => ArraySeq(Key(value))
    aggr match
      case Aggr.and => path                  -> ArraySeq.empty
      case Aggr.or  => ArraySeq.empty[ktype] -> ArraySeq(value)

  /** Merge two stores. stores with same key will have their sets merged
    *
    * @param a
    * @param b
    * @return
    */
  def merge(a: Store, b: Store): Store = b.foldLeft(a):
    case (acc, (k, v)) =>
      acc.get(k) match
        case Some(existing) => acc.updated(k, existing ++ v)
        case None           => acc.updated(k, v)
