package kubrick.prelude.lem

import kubrick.prelude.lem.all.*
object IHeadTail:
  trait IHeadTail[F[_]]:
    def headOption[T](a: F[T]): Option[NLem[T]]
    def tail[T](a: F[T]): F[T]
    def head[T](a: F[T]): NLem[T] = headOption(a).get
