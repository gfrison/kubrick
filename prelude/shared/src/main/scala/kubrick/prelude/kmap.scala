package kubrick.prelude

import cats.*
import scala.collection.immutable.HashMap
import traits.*
import cats.syntax.all.*

object kmap:

  case class Kmap[+K, +V] private[prelude] (
      nodes: HashMap[Any, Kmap.Node[Any, Any]],
      first: Option[Any],
      last: Option[Any]
  )
  given IPlus2[Kmap]:
    def containsKey[A,B](map:Kmap[A,B], key: A): Boolean = map.nodes.contains(key)
    def contains[A, B](a: Kmap[A, B], b: (A, B)): Boolean = 
      val (key,value) = b
      a.nodes.get(key) match
        case Some(node) => node.value == value
        case None       => false
    def get[A, B](a: Kmap[A, B], key: A): Option[B] =
      a.nodes.get(key) match
        case Some(node) => Some(node.value.asInstanceOf[B])
        case None       => None
    def plus[A,B](a:Kmap[A,B], pair: (A, B)): Kmap[A, B] =
      val (key, value) = pair
      if a.nodes.contains(key) then
        // Update existing: Replace value, preserve links
        val oldNode = a.nodes(key)
        val newNode = oldNode.copy(value = value)
        Kmap(a.nodes.updated(key, newNode), a.first, a.last)
      else
        // New insertion: Link to the current 'last'
        val newNode = Kmap.Node(value, prev = a.last, next = None)
        val updatedNodes = a.last match
          case Some(lastKey) =>
            a.nodes.get(lastKey) match
              case Some(prevLast) =>
                a.nodes
                  .updated(lastKey, prevLast.copy(next = Some(key)))
                  .updated(key, newNode)
              case None => a.nodes.updated(key, newNode)
          case None =>
            a.nodes.updated(key, newNode) 
        val nextFirst = if a.first.isEmpty then Some(key) else a.first
        Kmap(updatedNodes, nextFirst, Some(key))

  // Type classes
  given ITailHead2[Kmap]:

    def headOption[A,B](a: Kmap[A, B]): Option[(A, B)] =
      a.first match
        case Some(k) =>
          val node = a.nodes(k)
          Some((k.asInstanceOf[A], node.value.asInstanceOf[B]))
        case None => None

    def tail[A, B](a: Kmap[A, B]): Kmap[A, B] =
      a.first match
        case Some(k) =>
          val node         = a.nodes(k)
          val nextFirstKey = node.next
          val remainingNodes = nextFirstKey match
            case Some(nk) =>
              a.nodes.get(nk) match
                case Some(nextNode) =>
                  (a.nodes - k).updated(nk, nextNode.copy(prev = None))
                case None => a.nodes - k
            case None => HashMap.empty

          val nextLast = if nextFirstKey.isEmpty then None else a.last
          Kmap(remainingNodes, nextFirstKey, nextLast)
        case None =>
          throw new UnsupportedOperationException("tail of empty Kmap")



  given [K] => Traverse[[V] =>> Kmap[K, V]]:
    override def foldLeft[A, B](fa: Kmap[K, A], b: B)(f: (B, A) => B): B =
      @scala.annotation.tailrec
      def loop(current: Option[Any], acc: B): B =
        current match
          case None => acc
          case Some(k) =>
            val node = fa.nodes(k)
            loop(node.next, f(acc, node.value.asInstanceOf[A]))
      loop(fa.first, b)

    override def foldRight[A, B](fa: Kmap[K, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      def loop(current: Option[Any]): Eval[B] =
        current match
          case None => lb
          case Some(k) =>
            val node = fa.nodes(k)
            f(node.value.asInstanceOf[A], Eval.defer(loop(node.next)))
      loop(fa.first)

    def traverse[F[_]: Applicative, A, B](fa: Kmap[K, A])(f: A => F[B]): F[Kmap[K, B]] =
      import cats.syntax.all.*
      @scala.annotation.tailrec
      def loop(current: Option[Any], acc: F[Kmap[K, B]]): F[Kmap[K, B]] =
        current match
          case None => acc
          case Some(k) =>
            val node    = fa.nodes(k)
            val nextAcc = (acc, f(node.value.asInstanceOf[A])).mapN((map, newV) => map + (k.asInstanceOf[K] -> newV))
            loop(node.next, nextAcc)
      loop(fa.first, Applicative[F].pure(Kmap.empty[K, B]))

  given Bitraverse[Kmap]:
    override def bifoldLeft[A, B, C](fab: Kmap[A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C =
      @scala.annotation.tailrec
      def loop(current: Option[Any], acc: C): C =
        current match
          case None => acc
          case Some(k) =>
            val node = fab.nodes(k)
            loop(node.next, g(f(acc, k.asInstanceOf[A]), node.value.asInstanceOf[B]))
      loop(fab.first, c)

    override def bifoldRight[A, B, C](fab: Kmap[A, B], c: Eval[C])(
        f: (A, Eval[C]) => Eval[C],
        g: (B, Eval[C]) => Eval[C]
    ): Eval[C] =
      def loop(current: Option[Any]): Eval[C] =
        current match
          case None => c
          case Some(k) =>
            val node = fab.nodes(k)
            f(k.asInstanceOf[A], g(node.value.asInstanceOf[B], Eval.defer(loop(node.next))))
      loop(fab.first)

    def bitraverse[F[_]: Applicative, A, B, C, D](fab: Kmap[A, B])(
        f: A => F[C],
        g: B => F[D]
    ): F[Kmap[C, D]] =
      import cats.syntax.all.*
      @scala.annotation.tailrec
      def loop(current: Option[Any], acc: F[Kmap[C, D]]): F[Kmap[C, D]] =
        current match
          case None => acc
          case Some(k) =>
            val node = fab.nodes(k)
            val nextAcc =
              (acc, f(k.asInstanceOf[A]), g(node.value.asInstanceOf[B])).mapN((map, newK, newV) => map + (newK -> newV))
            loop(node.next, nextAcc)
      loop(fab.first, Applicative[F].pure(Kmap.empty[C, D]))

  object Kmap:
    // Internal node handles the doubly-linked pointers
    private[kmap] case class Node[+V, +K](
        value: V,
        prev: Option[Any],
        next: Option[Any]
    )

    def empty[K, V]: Kmap[K, V] = Kmap(HashMap.empty, None, None)

    def apply[K, V](elems: (K, V)*): Kmap[K, V] =
      elems.foldLeft(empty[K, V])((m, pair) => m + pair)

    def from[K, V](elems: Iterable[(K, V)]): Kmap[K, V] =
      elems.foldLeft(empty[K, V])((m, pair) => m + pair)

