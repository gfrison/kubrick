package kubrick.prelude.lem
import core.*
import kubrick.prelude.kset.{*, given}
import kubrick.prelude.traits.*
object plus:
  given IPlus[Lem]:
    def contains[T](a: Lem[T], b: T): Boolean = a match
      case L1(aa) => aa == b

    def plus[T <: Matchable](a: Lem[T], b: T): Lem[T] = (a, b) match
      case (bag: Bag[T], _)   => ibag.plus(bag, b)
      case (L1(aa), (k:T, v:T))   => 
        val p = Kset[Int](1,2) ++ Kset[Int](3,4)
        Bagdict(Kit1(L1(aa)), Pair(k, v))
      case (D2(dict), (k, v)) => D2(dict + (Lem(k) -> Lem(v)))
      case (L1(aa), _)        => Bag(aa, b)
  given ibag: IPlus[Bag]:
    def plus[T <: Matchable](a: Bag[T], b: T): Bag[T] = (a, b) match
      case (b2: B2[T], lb: T) => b2.copy(bag = b2.bag + Lem[T](lb))
    def contains[T](a: Bag[T], b: T): Boolean = ???
  extension [T <: Matchable](a:Lem[T])
    infix def ++(b: Lem[T]): Lem[T] = (a,b) match
      case (la: L1[T], lb: L1[T]) => Bag(la.value, lb.value)
      case (B2[T](b1:Kset[Lem[T]]), B2[T](b2:Kset[Lem[T]]))       => new B2[T](b1 ++ b2) 
    

