package kubrick.prelude

import scala.util.NotGiven

object traits:

  trait IPrepend[F[_], I[_]]:
    type Out[_]
    def prepend[T](a: F[T], b: I[T]): Out[T]
  extension [F[_], I[_], T](a: F[T])
    def +:[O[_]](b: I[T])(using p: IPrepend[F, I] { type Out[X] = O[X] }): O[T] = p.prepend(a, b)
  trait IAppend[F[_], I[_]]:
    type Out[_]
    def append[T](a: F[T], b: I[T]): Out[T]
  extension [F[_], I[_], T](a: F[T])
    def :+[O[_]](b: I[T])(using p: IAppend[F, I] { type Out[X] = O[X] }): O[T] = p.append(a, b)
  trait IGet[F[_], K[_]]:
    type Out[_]
    def containsKey[T, KK[*] <: K[*]](a: F[T], key: KK[T]): Boolean
    def get[T, KK[*] <: K[*]](a: F[T], key: KK[T]): Option[Out[T]]
  extension [F[_], K[_], T](a: F[T])
    def get[O[_], KK[*] <: K[*]](key: KK[T])(using g: IGet[F, K] { type Out[X] = O[X] }): Option[O[T]] = g.get(a, key)
    def containsKey[KK[*] <: K[*]](key: KK[T])(using g: IGet[F, K]): Boolean = g.containsKey(a, key)

  trait IPlus[F[_]]:
    def plus[T](a: F[T], b: T): F[T]
    def contains[T](a: F[T], b: T): Boolean
  extension [F[_], T](a: F[T])
    infix def +(b: T)(using p: IPlus[F]): F[T] = p.plus(a, b)
    infix def contains(b: T)(using p: IPlus[F]): Boolean                   = p.contains(a, b)
  trait IPlus2[F[_, _]]:
    def plus[A, B](a: F[A, B], b: (A, B)): F[A, B]
    def contains[A, B](a: F[A, B], b: (A, B)): Boolean
    def containsKey[A, B](a: F[A, B], key: A): Boolean
    def get[A, B](a: F[A, B], key: A): Option[B]
  extension [F[_, _], A, B](a: F[A, B])(using p: IPlus2[F])
    infix def +(b: (A, B))(using DummyImplicit): F[A, B] = p.plus(a, b)
    infix def +(k: A, v: B): F[A, B]                     = p.plus(a, (k, v))
    infix def contains(b: (A, B)): Boolean               = p.contains(a, b)
    infix def containsKey(key: A): Boolean               = p.containsKey(a, key)
    infix def get(key: A): Option[B]                     = p.get(a, key)
  trait ITailHead[F[_]]:
    def headOption[T](a: F[T]): Option[T]
    def tail[T](a: F[T]): F[T]
  extension [F[_], T](a: F[T])(using th: ITailHead[F])
    def headOption: Option[T] = th.headOption(a)
    def head: T               = th.headOption(a).get
    def tail: F[T]            = th.tail(a)
    def isEmpty: Boolean      = th.headOption(a).isEmpty
    def size: Int =
      @scala.annotation.tailrec
      def loop(current: F[T], acc: Int): Int =
        th.headOption(current) match
          case None    => acc
          case Some(_) => loop(th.tail(current), acc + 1)
      loop(a, 0)
  trait ITailHead2[F[_, _]]:
    def headOption[A, B](a: F[A, B]): Option[(A, B)]
    def tail[A, B](a: F[A, B]): F[A, B]
  extension [F[_, _], A, B](a: F[A, B])(using th: ITailHead2[F])
    def headOption: Option[(A, B)] = th.headOption(a)
    def head: (A, B)               = th.headOption(a).get
    def tail: F[A, B]              = th.tail(a)
    def isEmpty: Boolean           = th.headOption(a).isEmpty
    def size: Int =
      @scala.annotation.tailrec
      def loop(current: F[A, B], acc: Int): Int =
        th.headOption(current) match
          case None    => acc
          case Some(_) => loop(th.tail(current), acc + 1)
      loop(a, 0)
  extension [F[_], T](a: F[T])(using th: ITailHead[F], iplus: IPlus[F])
    def ++(b: F[T]): F[T] =
      @scala.annotation.tailrec
      def loop(current: F[T], acc: F[T]): F[T] =
        th.headOption(current) match
          case None    => acc
          case Some(h) => loop(th.tail(current), iplus.plus(acc, h))
      loop(b, a)
  extension [F[_, _], A, B](a: F[A, B])(using th: ITailHead2[F], iplus2: IPlus2[F])
    def ++(b: F[A, B]): F[A, B] =
      @scala.annotation.tailrec
      def loop(current: F[A, B], acc: F[A, B]): F[A, B] =
        th.headOption(current) match
          case None    => acc
          case Some(h) => loop(th.tail(current), iplus2.plus(acc, h))
      loop(b, a)

  // extractors
  object `+`:
    def unapply[F[_], T](a: F[T])(using p: IPlus[F], ht: ITailHead[F]): Option[(T, F[T])] =
      ht.headOption(a)
        .map: head =>
          head -> ht.tail(a)
    def unapply[F[_, _], A, B](
        m: F[A, B]
    )(using p: IPlus2[F], ht: ITailHead2[F]): Option[((A, B), F[A, B])] =
      m.headOption.map: head =>
        head -> ht.tail(m)
