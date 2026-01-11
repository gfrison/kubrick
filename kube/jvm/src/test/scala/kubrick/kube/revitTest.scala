package kubrick.kube

import kubrick.kube.all.*
import kubrick.prelude.all.{*, given}
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import scribe.*

import scala.collection.immutable.ArraySeq
class revitTest extends AsyncFreeSpec with Matchers:
  Logger.root.withMinimumLevel(Level.Trace).replace()
  "simple sek" in:
    val k = Kube(
      seqs = ArraySeq(M2m("a" -> Kid(1)), M2m("b" -> Kid(1))),
      roots = Set(Kid(1))
    )
    k.get(Kid(1)) should contain(Sek("a", "b"))
  "nested sek" in:
    val k = Kube(
      seqs = ArraySeq(M2m("a" -> Kid(1), "a2" -> Kid(2)), M2m(Kid(2) -> Kid(1), "b2" -> Kid(2))),
      roots = Set(Kid(1))
    )
    k.get(Kid(1)) should contain(Sek(L1("a"), Sek(L1("a2"), L1("b2"))))
  "set" in:
    val k = Kube(
      keys = M2m("a" -> Kid(1), "b" -> Kid(1)),
      roots = Set(Kid(1)),
      sets = Set(Kid(1))
    )
    k.get(Kid(1)) should contain(L0 + "a" + "b")
  "choice" in:
    val k = Kube(
      keys = M2m("a" -> Kid(1), "b" -> Kid(1)),
      roots = Set(Kid(1))
    )
    k.get(Kid(1)) should contain(L0 || "a" || "b")
  "sek in choice" in:
    val k = Kube(
      seqs = ArraySeq(M2m("a" -> Kid(1)), M2m("b" -> Kid(1))),
      keys = M2m(Kid(1) -> Kid(2), "c" -> Kid(2)),
      roots = Set(Kid(2)),
      sets = Set(Kid(2))
    )
    k.get(Kid(2)) should contain(Sek.from(ArraySeq.empty, Set(Sek("a", "b"), L1("c"))))
  "set and sek" in:
    val k = Kube(
      seqs = ArraySeq(M2m("a" -> Kid(1)), M2m("b" -> Kid(1))),
      keys = M2m("c" -> Kid(1)),
      roots = Set(Kid(1))
    )
    k.get(Kid(1)) should contain(Sek("a", "b") + "c")
  "pair" in:
    val k = Kube(
      keys = M2m("a" -> Kid(2)),
      vals = M2m("b" -> Kid(2)),
      roots = Set(Kid(2))
    )
    k.get(Kid(2)) should contain(Pair(L1("a"), L1("b")))
  "sek + choice" in:
    val k = Kube(
      seqs = ArraySeq(M2m("a" -> Kid(1)), M2m("b" -> Kid(1))),
      keys = M2m("c" -> Kid(1), "d" -> Kid(1)),
      roots = Set(Kid(1))
    )
    val sek = Sek("a", "b") + Choice("c", "d")
    debug { s"Expected sek: $sek" }
    k.get(Kid(1)) should contain(sek)