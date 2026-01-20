package kubrick.prelude.lem
import kubrick.prelude.traits.*
import kubrick.prelude.kset.{*, given}
import core.*
object headTail:
  given [T] => IHeadTail[B2, Lem]:
    type Out[X] = Bag[X]
    def headOption[T](a: B2[T]): Option[Lem[T]] =
      summon[IHeadTail[Kset, [X] =>> X]].headOption(a.bag)
    def isEmpty[T](a: B2[T]) = a.bag.isEmpty
    def size[T](a: B2[T])    = a.bag.size
    def tail[T](a: B2[T]) = 
      val ht = summon[IHeadTail[Kset, [X] =>> X]]
      new B2(ht.tail(a.bag))
