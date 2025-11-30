package kubrick.kube

import kubrick.prelude.all.{*, given}
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import scribe.*

import db.*

class dbTest extends AsyncFreeSpec with Matchers:
  Logger.root.withMinimumLevel(Level.Debug).replace()
  "doc retrieval" - {
    "from L1" in:
      DB(L1("a")).docSearch(L1("a")) shouldBe LazyList(L1("a"))
    "from sek of l1" in:
      val doc = Sek(L1("a"), L1("b"))
      DB(doc).docSearch(L1("a")) shouldBe LazyList(doc)
    "from sek of l1 query sek" in:
      val doc = Sek("a", "b", "c")
      DB(doc).docSearch(Sek("a", "b")) shouldBe LazyList(doc)
    "from pair within sek" in:
      val doc = Sek(Pair(L1("key1"), L1("val1")), Pair(L1("key2"), L1("val2")))
      DB(doc).docSearch(Pair(L1("key1"), L1("val1"))) shouldBe LazyList(doc)
    "sek2" in:
      val doc = (("a" +: ("b" --> "c")) :+ "d") :+ Sek("e", "f")
      val db  = DB(doc)
      db.docSearch("b" --> "c") should contain only (doc)
      db.docSearch("a") should contain only (doc)
      db.docSearch("a" +: "d" +: L0) should contain only (doc)
    "sek3" in:
      val doc = (("a" +: ("b" --> "c")) :+ "d") :+ Sek("e", "f")
      val db  = DB(doc)
      db.docSearch(Sek(L1("a"), L1("d"), Sek("e", "f"))) should contain only (doc)
      db.docSearch(Sek("a", "d")) should contain only (doc)
      db.docSearch("b" --> "c") should contain only (doc)
      db.docSearch("b" --> "ccc") shouldBe empty
    // "choice1" in:
    //   val doc = Choice("a", "b")
    //   val db  = DB(doc)
    //   db.docSearch("a") should contain only (doc)
    //   db.docSearch("b") should contain only (doc)
    //   db.docSearch("c") shouldBe empty
  }
