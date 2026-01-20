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

import kubrick.prelude.kmap.{*, given}
import kubrick.prelude.kset.{*, given}
import kubrick.prelude.traits.*

import scala.collection.immutable.ArraySeq
import scala.util.NotGiven
object core:
  sealed trait Lem[+T] extends Matchable
  // sealed trait L2[+K,+V] extends Lem[(K, V)]
  case object L0                                                   extends Lem[Nothing]
  case class L1[+T](value: T)(using NotGiven[T <:< Lem[?]])        extends Lem[T]
  case class Pair[+T] private[lem] (key: Lem[T], value: Lem[T])    extends Dict[T]
  sealed trait Kit[+T]                                             extends Lem[T]
  sealed trait Sek[+T]                                             extends Kit[T]
  sealed trait Bag[+T]                                             extends Kit[T]
  sealed trait Dict[+T]                                            extends Kit[T]
  sealed trait Choice[+T]                                          extends Bag[T]
  case class B2[+A] private[lem] (bag: Kset[Lem[A]])               extends Bag[A]
  case class C2[+T] private[lem] (bag: Kset[Lem[T]])               extends Choice[T]
  case class S2[+T] private[lem] (sek: Vector[Lem[T]])             extends Sek[T]
  case class Sekdict[+T] private[lem] (sek: Sek[T], dict: Dict[T]) extends Sek[T] with Dict[T]
  case class Bagdict[+T] private[lem] (bag: Bag[T], dict: Dict[T]) extends Bag[T] with Dict[T]
  case class D2[+T] private[lem] (dict: Kmap[Lem[T], Lem[T]])      extends Dict[T]
  case class Kit1[+T] private[lem] (lem: Lem[T])                   extends Sek[T] with Bag[T]

  object S2:
    def apply[T](fst: T, snd: T, rest: T*)(using NotGiven[T =:= Lem[?]]): S2[T] =
      new S2((Vector(fst, snd) ++ rest.toVector).map(L1(_)))
    def unapplySeq[T](sek: S2[T]): Option[Vector[Lem[T]]] = Some(sek.sek)

  object Bag:
    def apply[T <: Matchable](fst: T, snd: T, others: T*)(using NotGiven[T =:= Lem[?]]): Bag[T] =
      B2(Kset.from((fst +: snd +: others).map(Lem(_))))
    def unapplySeq[T](bag: Bag[T]): Option[List[Lem[T]]] = bag match
      case b2: B2[T] => Kset.unapply(b2.bag)
      case Kit1(el)  => Some(List(el))
      case _         => None

  object Sek:
    def apply[T <: Matchable](fst: T, snd: T, others: T*)(using NotGiven[T =:= Lem[?]]): Sek[T] =
      apply[T](Lem(fst), Lem(snd), others.toList.map(Lem(_))*)
    def apply[T](fst: Lem[T], snd: Lem[T], others: Lem[T]*): Sek[T] = S2(Vector(fst, snd) ++ others.toVector)
    def unapplySeq[T](sek: Sek[T]): Option[Vector[Lem[T]]] = sek match
      case s2: S2[T] => Some(s2.sek)
      case Kit1(el)  => Some(Vector(el))
      case _         => None

  object Pair:
    def apply[T](key: T, value: T)(using NotGiven[T <:< Lem[?]]): Pair[T] = Pair(Lem(key), Lem(value))
    def unapply[T](pair: Pair[T]): Option[(Lem[T], Lem[T])]               = Some((pair.key, pair.value))

  object Choice:
    def apply[T <: Matchable](fst: T, snd: T, others: T*)(using NotGiven[T =:= Lem[?]]): Choice[T] =
      apply(Lem(fst), Lem(snd), others.toList.map(Lem(_))*)
    def apply[T](fst: Lem[T], snd: Lem[T], others: Lem[T]*): Choice[T] = C2(Kset((fst +: snd +: others)*))

  // object --> {
  //   def unapply[T](lem: Lem[T]): Option[(Lem[T], Lem[T])] = lem match
  //     case pair: Pair[T] => Some((pair.key, pair.value))
  //     case _             => None
  // }

  object Dict:
    def apply[T <: Matchable](fst: (T, T), snd: (T, T), others: (T, T)*)(using NotGiven[T =:= Lem[?]]): Dict[T] =
      D2(Kmap.from((fst +: snd +: others).map { case (k, v) => (Lem(k), Lem(v)) }))

  object B2:
    def apply[A <: Matchable](fst: A, snd: A, others: A*)(using NotGiven[A =:= Lem[?]]): B2[A] =
      apply(Lem(fst), Lem(snd), others.toList.map(Lem(_))*)
    def apply[A](fst: Lem[A], snd: Lem[A], others: Lem[A]*): B2[A] =
      new B2[A](Kset.from(fst +: snd +: others))

  object D2:
    def empty[T]: D2[T] = new D2(Kmap.empty[Lem[T], Lem[T]])
    def apply[T](fst: (T, T), snd: (T, T), others: (T, T)*)(using NotGiven[T =:= Lem[?]]): D2[T] =
      import kubrick.prelude.traits.*
      val pairs    = (fst +: snd +: others).map(Pair(_, _))
      val finalMap = pairs.foldLeft(Kmap.empty[Lem[T], Lem[T]])((map, p) => map + (p.key -> p.value))
      new D2(finalMap)
    // def unapply[T](m: D2[T]): Option[(Pair[T], D2[T])] =
    //   if m.dict.isEmpty then None
    //   else
    //     val (k, v) = m.dict.head
    //     Some(Pair(k, v), new D2(m.dict.tail))

  object Lem:
    def apply[T](seq: Seq[T])(using NotGiven[T =:= Lem[?]]): Lem[T] = seq.size match
      case 0 => L0
      case 1 => L1(seq.head)
      case _ => S2(seq.head, seq(1), seq.drop(2)*)

    def apply[T <: Matchable](set: Set[T])(using NotGiven[T =:= Lem[?]], DummyImplicit): Lem[T] = set.size match
      case 0 => L0
      case 1 => L1(set.head)
      case _ => Bag(set.head, set.tail.head, set.toSeq.drop(2)*)
    def apply[T <: Matchable](map: Map[T, T])(using NotGiven[T =:= Lem[?]], DummyImplicit, DummyImplicit): Lem[T] =
      map.size match
        case 0 => L0
        case 1 => Pair(map.head._1, map.head._2)
        case _ => Dict(map.head, map.tail.head, map.toSeq.drop(2)*)
    def apply[T <: Matchable](el: Matchable): Lem[T] = el match
      case seq: Seq[T]    => apply(seq)
      case set: Set[T]    => apply(set)
      case pair: (T, T)   => Pair(pair._1, pair._2)
      case map: Map[T, T] => apply(map)
      case lem: Lem[T]    => lem
      case value: T       => L1(value)
