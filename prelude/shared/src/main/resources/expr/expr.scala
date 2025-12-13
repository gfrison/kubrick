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

package kubrick.expr

import cats.Show
import cats.syntax.all.*
import scribe.Logging
import scala.util.NotGiven
import kubrick.prelude.all.*
object core:
  trait Expr
  trait Clause extends Expr
  trait Fun    extends Expr
  trait MethodX:
    val claim: Sek[Term]
  case object FailK extends Fun with Clause
  type FailK = Expr
  case class AtomX(value: Atom)                extends Fun
  case class VarX(value: Var)                  extends Fun
  case class Assign(head: Var, body: Lem[Fun]) extends Clause
  enum AggrOp extends Enum[AggrOp]:
    case sum, avg, count, max, min
  case class AggrX(op: AggrOp, from: Lem[Fun], groupBy: Option[Var] = None) extends Fun
  case class SimpleFun(val claim: Sek[Term], body: Lem[Expr])               extends MethodX with Expr
  case class Implication(val claim: Sek[Term], premises: Lem[Clause])       extends MethodX with Expr
  case class Persist(
      val claim: Sek[Term],
      premises: Lem[Clause],
      selection: Set[Comb] = Set.empty
  ) extends Expr
  case class FilterClause(condition: Lem[Clause]) extends Expr
  case class Query(value: Lem[Expr])              extends Expr
  case class FactConstraint(value: Sek[Atom])     extends Expr
  enum ObjKind:
    case >, >>, <, <<
  case class Objective(kind: ObjKind, body: Lem[Expr]) extends Expr
  case class Binomial(selection: Int, v: Option[Var] = None)
  case class Call(funs: Sek[Fun]) extends Fun with Clause
  enum FilterOp extends Enum[FilterOp]:
    case `==`, `!=`, `>`, `<`, `>=`, `<=`, `in`, `not in`, `like`, `not like`, `ilike`, `not ilike`
  case class Filter(op: FilterOp, body: Lem[Fun]) extends Clause
  enum NativeOp extends Enum[NativeOp]:
    case `+`, `-`, `*`, `/`, `%`, `&`, `|`, `^`, `<<`, `>>`, `||`, `&&`, `~`, `!`
  case class Native(op: NativeOp, body: Lem[Fun]) extends Fun
  case class Prefix(ns: String, uri: Uri)

  trait Comb:
    val varr: Var
    override def hashCode(): Int = varr.hashCode()
    override def equals(obj: Any): Boolean = obj match
      case x: Comb => varr == x.varr
      case _       => false
  case class Rep(varr: Var, min: Int = 1, max: Int = 1)  extends Comb
  case class Part(varr: Var, min: Int = 1, max: Int = 1) extends Comb
  object Assign:
    def apply(head: Var, arg: Lem[Fun]) = new Assign(head, arg)
    def apply(head: Var, arg: Fun)      = new Assign(head, L1(arg))
    def apply(head: Var, arg: Term) = new Assign(
      head,
      L1(arg match
        case v: Var  => VarX(v)
        case a: Atom => AtomX(a))
    )
    def apply(head: Var, arg0: Term, arg1: Term, args: Term*) = new Assign(
      head,
      (arg0 +: arg1 +: args).toList match
        case (v: Var) :: Nil  => L1(VarX(v))
        case (a: Atom) :: Nil => L1(AtomX(a))
        case a :: b :: t      => L1(Call(a, b, t*))
    )
  object AggrX:
    def apply(op: AggrOp, from: Fun | Term) = new AggrX(
      groupBy = None,
      op = op,
      from = L1(from match
        case v: Var  => VarX(v)
        case f: Fun  => f
        case a: Atom => AtomX(a))
    )
    def apply(op: AggrOp, arg0: (Fun | Term), arg1: (Fun | Term), args: (Fun | Term)*): AggrX =
      new AggrX(groupBy = None, op = op, from = L1(Call(arg0, arg1, args*)))
  object Filter:
    def apply(op: FilterOp, body: Lem[Fun]): Filter = new Filter(op, body)
    def apply(op: FilterOp, fst: (Atom | Var | Fun), els: (Atom | Var | Fun)*): Filter =
      val ls = if els.isEmpty then Seq(fst) else fst +: els
      new Filter(
        op,
        Sek(ls.map {
          case v: Var => VarX(v)
          case f: Fun => f
          case a: T   => AtomX(a)
        })
      )
    def apply(op: String, fst: (Atom | Var | Fun), els: (Atom | Var | Fun)*)(using
        NotGiven[T <:< Lem]
    ): Filter =
      val pop = FilterOp.valueOf(op)
      apply(pop, fst, els*)
  object Native:
    def apply(op: NativeOp, body: Lem[Fun]): Native = new Native(op, body)
    def apply(op: NativeOp, fst: (Atom | Var | Fun), els: (Atom | Var | Fun)*): Native =
      val ls = if els.isEmpty then Seq(fst) else fst +: els
      new Native(
        op,
        Sek(ls.map {
          case v: Var  => VarX(v)
          case f: Fun  => f
          case a: Atom => AtomX(a)
        })
      )
    def apply(op: String, fst: (Atom | Var | Fun), els: (Atom | Var | Fun)*): Native =
      val pop = NativeOp.valueOf(op)
      apply(pop, fst, els*)
  object Call extends Logging:
    type all = Lem[Fun] | Fun | Term | (Atom | Var, Term | Fun)
    def from(sek: Sek[Fun]) = new Call(sek)
    def apply(arg: all) = new Call(Sek(arg match
      case (k: Var, v: Var)  => Two(VarX(k), VarX(v))
      case (k: Var, v: Fun)  => Two(VarX(k), v)
      case (k: Var, v: Atom) => Two(VarX(k), AtomX(v))
      case (k: T, v: Var)    => Two(AtomX(k), VarX(v))
      case (k: T, v: Fun)    => Two(AtomX(k), v)
      case (k: T, v: Atom)   => Two(AtomX(k), AtomX(v))
      case v: Var            => L1(VarX(v))
      case f: Fun            => L1(f)
      case a: T              => L1(AtomX(a))))
    def apply(arg0: all, arg1: all, args: all*): Call = new Call(
      Sek(
        { arg0 +: arg1 +: args }.map {
          case kg: Lem[Fun]      => kg
          case a: Fun            => L1[Fun](a)
          case (k: Var, v: Var)  => Two(VarX(k), VarX(v))
          case (k: Var, v: Fun)  => Two(VarX(k), v)
          case (k: Var, v: Atom) => Two(VarX(k), AtomX(v))
          case (k: T, v: Var)    => Two(AtomX(k), VarX(v))
          case (k: T, v: Fun)    => Two(AtomX(k), v)
          case (k: T, v: Atom)   => Two(AtomX(k), AtomX(v))
          case a: Var            => L1(VarX(a))
          case a: T              => L1(AtomX(a))
        }
      )
    )

  given [T: Show] => Show[AtomX] = (a: AtomX) => a.value.show
  given [T: Show] => Show[VarX]  = (a: VarX) => a.value.show
  given Show[FailK.type]         = _ => "fail"
  given [T: Show] => Show[Expr] = {
    case a: Fun    => Fun.show(a)
    case a: Clause => Clause.show(a)
  }
  given Clause: [T: Show] => Show[Clause] = {
    case a: Assign => assign.show(a)
    case a: Call   => callk.show(a)
    case a: Filter => a.show
  }
  given Fun: [T: Show] => Show[Fun] = {
    case a: AggrX  => aggrk.show(a)
    case a: Call   => callk.show(a)
    case FailK     => "fail"
    case a: AtomX  => a.show
    case a: VarX   => a.show
    case x: Native => x.show
  }
  given [T: Show] => Show[Term] = _ match
    case v: Var => v.show
    case t: T   => t.show
  given callk: [T: Show] => Show[Call]    = (c: Call) => c.funs.show // summon[Show[Lem[Fun]]].show(c.funs)
  given assign: [T: Show] => Show[Assign] = (a: Assign) => s"${a.head.show}<-${a.body.show}"
  given aggrk: [T: Show] => Show[AggrX] = (a: AggrX) =>
    s"group ${a.op} ${a.from.show}${a.groupBy.fold("")(g => s" groupBy<-${g.show}")}"
  given sfk: [T: Show] => Show[SimpleFun] = (a: SimpleFun) => s"${a.claim.show} = ${a.body.show}"
  given [T: Show] => Show[Implication]    = (a: Implication) => s"${a.claim.show} -| ${a.premises.show}"
  given [T: Show] => Show[FilterClause]   = a => a.condition.show
  given [T: Show] => Show[Persist]        = p => s"${p.claim.show} -| ${p.premises.show}"
  given [T: Show] => Show[Filter]         = f => s"(${f.op} ${f.body.show})"
  given [T: Show] => Show[Native]         = f => s"(${f.op} ${f.body.show})"
  given [T: Show] => Show[Query]          = q => s"?${q.value.show}"
  given [T: Show] => Show[Objective]      = q => s"\\${q.kind} ${q.body.show}"
