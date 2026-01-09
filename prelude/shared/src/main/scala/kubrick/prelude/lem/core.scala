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

/** Represents the core functionality of the Kubrick Prelude Lem module.
  */
package kubrick.prelude.lem

import cats.syntax.all.*
import kubrick.prelude.cmap.*
import kubrick.prelude.cset.{*, given}

import scala.collection.immutable.ArraySeq
import scala.util.NotGiven

import choicer.{*, given}
object core:
  /** Sealed trait representing a Lem structure.
    * @tparam T
    *   the type of the elements in the Lem.
    */
  sealed trait Lem[+T]

  /** Sealed trait representing a non-empty Lem structure.
    * @tparam T
    *   the type of the elements in the NLem.
    */
  sealed trait NLem[+T] extends Lem[T]

  /** Represents an empty Lem.
    */
  case object L0 extends Lem[Nothing]

  /** Represents a single element Lem.
    * @param value
    *   the value contained in the Lem.
    */
  case class L1[+T](value: T) extends NLem[T]

  /** Represents a pair of Lems (key -> value).
    * @param left
    *   the key element.
    * @param right
    *   the value element.
    */
  case class Pair[+T](left: NLem[T], right: NLem[T]) extends NLem[T]

  /** Represents a sequence of Lems with optional pairs.
    * @param line
    *   the sequence of NLem elements.
    * @param pairs
    *   optional pairs associated with the sequence.
    */
  case class Sek[+T](line: ArraySeq[NLem[T]], pairs: Cmap[NLem[T], NLem[T]] = Cmap.empty) extends NLem[T]:
    export pairs.{get, containsKey}

  /** Represents a bag (set) of Lems, combining a set and pairs.
    * @param set
    *   the set of NLem elements.
    * @param pairs
    *   optional pairs associated with the bag.
    */
  case class Bag[+T](set: Cset[NLem[T]] = Cset.empty, pairs: Cmap[NLem[T], NLem[T]] = Cmap.empty)
      extends Lem[T]
      with NLem[T]:
    export pairs.get

    /** Checks if the bag contains a specific key.
      * @param key
      *   the key to check for.
      * @tparam V
      *   the type of the key.
      * @return
      *   true if the key is present, false otherwise.
      */
    def containsKey[V >: T](key: Lem[V]): Boolean = set.contains(key) || pairs.containsKey(key)

  /** Represents a choice (alternatives) of Lems.
    * @param values
    *   the set of possible Lem values.
    */
  case class Choice[+T](values: Cset[Lem[T]]) extends NLem[T]

  object Bag:
    /** Creates a Bag from two or more elements.
      * @param fst
      *   the first element.
      * @param snd
      *   the second element.
      * @param others
      *   additional elements.
      * @tparam T
      *   the type of the elements.
      * @return
      *   a new Bag containing the elements.
      */
    def apply[T](fst: T, snd: T, others: T*)(using NotGiven[T =:= Lem[?]]): Bag[T] =
      apply(Cset(L1(fst), L1(snd)), Cmap.empty, others.toList.map(L1(_)))

    /** Creates a Bag from two or more NLem elements.
      * @param fst
      *   the first NLem element.
      * @param snd
      *   the second NLem element.
      * @param others
      *   additional NLem elements.
      * @tparam T
      *   the type of the elements.
      * @return
      *   a new Bag containing the NLem elements.
      */
    def apply[T](fst: NLem[T], snd: NLem[T], others: NLem[T]*): Bag[T] =
      apply(Cset(fst, snd), Cmap.empty, others.toList)
    private def apply[T](set: Cset[NLem[T]], pairs: Cmap[NLem[T], NLem[T]], others: List[NLem[T]]): Bag[T] =
      others.toList match
        case Nil             => new Bag(set, pairs)
        case Pair(l, r) :: t => apply(set + l + r, pairs + (l, r), t)
        case L0 :: t         => apply(set, pairs, t)
        case h :: t          => apply(set + h, pairs, t)

    /** Unapplies a Bag to extract its contents.
      * @param bag
      *   the Bag to unapply.
      * @tparam T
      *   the type of the elements.
      * @return
      *   an Option containing the set and pairs of the Bag.
      */
    def unapplySeq[T](bag: Bag[T]): Option[(Cset[Lem[T]], Cmap[Lem[T], Lem[T]])] = Some((bag.set, bag.pairs))

  object Sek:
    /** Creates a Sek from two or more elements.
      * @param fst
      *   the first element.
      * @param snd
      *   the second element.
      * @param others
      *   additional elements.
      * @tparam T
      *   the type of the elements.
      * @return
      *   a new Sek containing the elements.
      */
    def apply[T](fst: T, snd: T, others: T*)(using NotGiven[T =:= Lem[?]]): Sek[T] =
      apply(ArraySeq(L1(fst), L1(snd)), Cmap.empty, others.toList.map(L1(_)))

    /** Creates a Sek from two or more NLem elements.
      * @param fst
      *   the first NLem element.
      * @param snd
      *   the second NLem element.
      * @param others
      *   additional NLem elements.
      * @tparam T
      *   the type of the elements.
      * @return
      *   a new Sek containing the NLem elements.
      */
    def apply[T](fst: NLem[T], snd: NLem[T], others: NLem[T]*): Sek[T] =
      apply(ArraySeq.empty[NLem[T]], Cmap.empty, fst :: snd :: others.toList)
    private def apply[T](seq: ArraySeq[NLem[T]], pairs: Cmap[NLem[T], NLem[T]], others: List[NLem[T]]): Sek[T] =
      others.toList match
        case Nil             => new Sek(seq, pairs)
        case Pair(l, r) :: t => apply(seq, pairs + (l, r), t)
        case L0 :: t         => apply(seq, pairs, t)
        case h :: t          => apply(seq :+ h, pairs, t)

    /** Unapplies a Sek to extract its contents.
      * @param sek
      *   the Sek to unapply.
      * @tparam T
      *   the type of the elements.
      * @return
      *   an Option containing the line and pairs of the Sek.
      */
    def unapplySeq[T](sek: Sek[T]): Option[(ArraySeq[Lem[T]], Cmap[Lem[T], Lem[T]])] = Some((sek.line, sek.pairs))

  object Choice:
    /** Creates a Choice from two or more elements.
      * @param fst
      *   the first element.
      * @param snd
      *   the second element.
      * @param others
      *   additional elements.
      * @tparam T
      *   the type of the elements.
      * @return
      *   a new Choice containing the elements.
      */
    def apply[T](fst: T, snd: T, others: T*)(using NotGiven[T =:= Lem[?]]): Choice[T] =
      (new Choice[T](Cset(L1(fst), L1(snd)))) || from(others.map(L1(_)).toList)

    /** Creates a Choice from a set of Lem values.
      * @param seq
      *   the set of Lem values.
      * @param others
      *   additional Lem values.
      * @tparam T
      *   the type of the elements.
      * @return
      *   a new Choice containing the Lem values.
      */
    def apply[T](seq: Cset[Lem[T]], others: List[Lem[T]]): Choice[T] =
      others.toList match
        case Nil    => new Choice(seq)
        case h :: t => apply(seq + h, t)

    /** Creates a Choice from two or more Lem elements.
      * @param fst
      *   the first Lem element.
      * @param snd
      *   the second Lem element.
      * @param others
      *   additional Lem elements.
      * @tparam T
      *   the type of the elements.
      * @return
      *   a new Choice containing the Lem elements.
      */
    def apply[T](fst: Lem[T], snd: Lem[T], others: List[Lem[T]]): Choice[T] = others.toList match
      case Nil    => new Choice(Cset(fst, snd))
      case h :: t => apply(Cset(fst, snd, h), t)

    /** Creates a Choice from two or more Lem elements.
      * @param fst
      *   the first Lem element.
      * @param snd
      *   the second Lem element.
      * @param others
      *   additional Lem elements.
      * @tparam T
      *   the type of the elements.
      * @return
      *   a new Choice containing the Lem elements.
      */
    def apply[T](fst: Lem[T], snd: Lem[T], others: Lem[T]*): Choice[T] = others.toList match
      case Nil    => new Choice(Cset(fst, snd))
      case h :: t => apply(Cset(fst, snd, h), t.toList)

    /** Converts a list of Lems into a single Lem structure.
      * @param ls
      *   the list of Lems.
      * @tparam T
      *   the type of the elements.
      * @return
      *   a single Lem structure representing the list.
      */
    def from[T](ls: List[Lem[T]]): Lem[T] = ls match
      case Nil      => L0
      case h :: Nil => h
      case h :: t   => h || from(t)

  object --> {

    /** Unapplies a Lem to extract its left and right components if it is a Pair.
      * @param lem
      *   the Lem to unapply.
      * @tparam T
      *   the type of the elements.
      * @return
      *   an Option containing the left and right components.
      */
    def unapply[T](lem: Lem[T]): Option[(Lem[T], Lem[T])] = lem match
      case pair: Pair[T] => Some((pair.left, pair.right))
      case _             => None
  }

  object +: {

    /** Unapplies a Sek to extract its head and tail components.
      * @param sek
      *   the Sek to unapply.
      * @tparam T
      *   the type of the elements.
      * @tparam S
      *   the type of the elements in the Sek.
      * @return
      *   an Option containing the head and tail components.
      */
    def unapply[T, S <: Lem[T]](sek: Sek[T]): Option[(Lem[T], Lem[T])] =
      sek.line.headOption.map: h =>
        h -> (sek.line.tail match
          case ArraySeq() if sek.pairs.isEmpty => L0
          case ArraySeq()                      => new Sek(ArraySeq.empty, sek.pairs)
          case ArraySeq(el)                    => new Sek(ArraySeq(el), sek.pairs)
          case _                               => new Sek(sek.line.tail, sek.pairs))

  }
  object :+ {

    /** Unapplies a Sek to extract its last and initial components.
      * @param sek
      *   the Sek to unapply.
      * @tparam T
      *   the type of the elements.
      * @tparam S
      *   the type of the elements in the Sek.
      * @return
      *   an Option containing the last and initial components.
      */
    def unapply[T, S <: Lem[T]](sek: Sek[T]): Option[(Lem[T], Lem[T])] =
      sek.line.lastOption.map: h =>
        h -> (sek.line.init match
          case ArraySeq() if sek.pairs.isEmpty => L0
          case ArraySeq()                      => new Sek(ArraySeq.empty, sek.pairs)
          case ArraySeq(el)                    => new Sek(ArraySeq(el), sek.pairs)
          case _                               => Sek(sek.line.init, sek.pairs))
  }
  object || {

    /** Unapplies a Lem to extract its components if it is a Choice.
      * @param lem
      *   the Lem to unapply.
      * @tparam T
      *   the type of the elements.
      * @return
      *   an Option containing the components of the Choice.
      */
    def unapply[T](lem: Lem[T]): Option[(Lem[T], Lem[T])] = lem match
      case Choice(values) =>
        values.headOption.map:
          case h: Lem[T] =>
            h -> {
              values.tail.size match
                case 1 => values.tail.headOption.get
                case 0 => L0
                case _ => new Choice(values.tail)
            }
      case _ => None
  }
  object + {

    /** Unapplies a Lem to extract its components if it is a Bag or Sek.
      * @param lem
      *   the Lem to unapply.
      * @tparam T
      *   the type of the elements.
      * @return
      *   an Option containing the components of the Bag or Sek.
      */
    def unapply[T](lem: Lem[T]): Option[(NLem[T], Lem[T])] = lem match
      case Bag[T](set, pairs) if set.nonEmpty =>
        set.headOption.map: head =>
          head -> {
            val newSet = set.tail
            newSet.size match
              case 1 => if pairs.isEmpty then newSet.headOption.get else new Bag(newSet, pairs)
              case 0 => if pairs.isEmpty then L0 else new Bag(newSet, pairs)
              case _ => new Bag(newSet, pairs)
          }
      case Sek[T](line, pairs) if pairs.nonEmpty =>
        pairs.headOption.map:
          case (k, v) =>
            Pair(k, v) -> {
              val tpairs = pairs.tail
              tpairs.size match
                case 1 => Pair(tpairs.head._1, tpairs.head._2)
                case _ => new Sek(line, tpairs)
            }
      case _ => None
  }
