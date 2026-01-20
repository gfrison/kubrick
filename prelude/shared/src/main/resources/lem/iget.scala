package kubrick.prelude.lem

import kubrick.prelude.traits.IGet
import core.*

object iget:
  // IGet for D2 - dictionary lookup by key
  given [T] => IGet[D2, Lem]:
    type Out[X] = Lem[X]
    def containsKey[T, KK[*] <: Lem[*]](a: D2[T], key: KK[T]): Boolean =
      a.dict.containsKey(key)
    def get[T, KK[*] <: Lem[*]](a: D2[T], key: KK[T]): Option[Lem[T]] =
      a.dict.get(key)

  // IGet for Pair - returns value if key matches
  given [T] => IGet[L2, Lem]:
    type Out[X] = Lem[X]
    def containsKey[T, KK[*] <: Lem[*]](a: L2[T], key: KK[T]): Boolean =
      a.key == key
    def get[T, KK[*] <: Lem[*]](a: L2[T], key: KK[T]): Option[Lem[T]] =
      if a.key == key then Some(a.value)
      else None

  // IGet for Dict - delegates to D2 or Pair instances
  given [T] => IGet[Dict, Lem]:
    type Out[X] = Lem[X]
    def containsKey[T, KK[*] <: Lem[*]](a: Dict[T], key: KK[T]): Boolean =
      a match
        case d2: D2[T]     => summon[IGet[D2, Lem]].containsKey(d2, key)
        case pair: L2[T] => summon[IGet[L2, Lem]].containsKey(pair, key)
        case _             => false
    def get[T, KK[*] <: Lem[*]](a: Dict[T], key: KK[T]): Option[Lem[T]] =
      a match
        case d2: D2[T]     => summon[IGet[D2, Lem]].get(d2, key)
        case pair: L2[T] => summon[IGet[L2, Lem]].get(pair, key)
        case _             => None
