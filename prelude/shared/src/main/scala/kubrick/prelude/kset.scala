package kubrick.prelude

import cats.*
import scala.collection.immutable.HashSet
import traits.*
import scala.util.NotGiven
import cats.syntax.all.*
import scala.compiletime.asMatchable
object kset:
  type Id[X] = X
  case class Kset[+A] private[prelude] (set: HashSet[Any] = HashSet.empty, seq: List[A] = List.empty)
    // infix def +[B >: A](elem: B): Kset[B] =
    //   if set.contains(elem) then this
    //   else new Kset(set + elem, elem :: seq)
    // infix def ++[B >: A](other: Kset[B]): Kset[B] = seq.foldLeft(other):
    //   case (acc, elem) => acc + elem
    // infix def contains[B >: A](elem: B): Boolean = set.contains(elem)

  // type classes
  given IPlus[Kset]:
    def plus[T <: Matchable](a: Kset[T], b: T): Kset[T] = 
      if a.set.contains(b) then a
      else new Kset(a.set + b, b :: a.seq)
    def contains[T](a: Kset[T], b: T): Boolean = a.set.contains(b)
    // def combine[T](a: Kset[T], b: Kset[T]): Kset[T] = a.seq.foldLeft(b):
    //   case (acc, elem) => 
    //     if acc.set.contains(elem) then acc
    //     else new Kset(acc.set + elem, elem :: acc.seq)

  given ITailHead[Kset]:
    def headOption[T](a: Kset[T]): Option[T] = a.seq.headOption
    def tail[T](a: Kset[T]) =
      val h = a.seq.head
      new Kset(a.set - h, a.seq.tail)
    // def isEmpty[T](a: Kset[T]): Boolean = a.seq.isEmpty
    // def size[T](a: Kset[T]): Int        = a.set.size
  // given MonoidK[Kset]:
  //   def combineK[A](x: Kset[A], y: Kset[A]): Kset[A] =
  //     y.seq.foldLeft(x) { (acc, elem) =>
  //       if acc.set.contains(elem) then acc
  //       else new Kset(acc.set + elem, elem :: acc.seq)
  //     }
  //   def empty[A]: Kset[A] = Kset.empty[A]
  given Traverse[Kset]:
    override def foldLeft[A, B](fa: Kset[A], b: B)(f: (B, A) => B): B =
      fa.seq.reverse.foldLeft(b)(f)
    override def foldRight[A, B](fa: Kset[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      fa.seq.foldRight(lb)(f)
    def traverse[F[_]: Applicative, A, B](fa: Kset[A])(f: A => F[B]): F[Kset[B]] =
      import cats.syntax.all.*
      fa.seq.reverse.traverse(f).map(bs => Kset.from(bs.map(_.asMatchable)))
  object Kset:
    def empty[A]: Kset[A]                                      = Kset()
    def pure[T](el: T)(using NotGiven[T <:< Kset[?]]): Kset[T] = Kset(set = HashSet(el), seq = List(el))
    def apply[A <: Matchable](elems: A*): Kset[A]                           = from(elems)
    def from[A <: Matchable](elems: Iterable[A]): Kset[A]                   = elems.foldLeft(empty)(_ + _)
    def unapply[A](kset: Kset[A]): Option[List[A]]             = Some(kset.seq.reverse)
