package kubrick.prelude

import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import scribe.*

import cset.*
import all.{*, given}
class csetTest extends AsyncFreeSpec with Matchers:
  Logger.root.withMinimumLevel(Level.Debug).replace()
  "+ " in:
    val cs = Cset.empty[Int] + 3 + 1 + 2
    cs.contains(1) shouldBe true
    cs.contains(2) shouldBe true
    cs.contains(3) shouldBe true
    cs.contains(4) shouldBe false

  "intersect" in:
    val cs1 = Cset(1, 2, 3, 4, 5)
    val cs2 = Cset(4, 7, 8, 5, 6)
    val cs3 = cs1.intersect(cs2)
    cs3.contains(4) shouldBe true
    cs3.contains(5) shouldBe true
    cs3.contains(1) shouldBe false
    cs3.contains(6) shouldBe false
  "diff" in:
    val cs1 = Cset(1, 2, 3, 4, 5)
    val cs2 = Cset(4, 7, 8, 5, 6)
    val cs3 = cs1.diff(cs2)
    cs3.contains(1) shouldBe true
    cs3.contains(2) shouldBe true
    cs3.contains(3) shouldBe true
    cs3.contains(4) shouldBe false
    cs3.contains(5) shouldBe false
  "++" in:
    val cs1 = Cset(1, 2, 3)
    val cs2 = Cset(3, 4, 5)
    val cs3 = cs1 ++ cs2
    cs3.contains(1) shouldBe true
    cs3.contains(2) shouldBe true
    cs3.contains(3) shouldBe true
    cs3.contains(4) shouldBe true
    cs3.contains(5) shouldBe true
  "map" in:
    Cset(1, 2, 3).map(_ * 2).toList shouldBe List(2, 4, 6)

  "empty set" in:
    val cs = Cset.empty[Int]
    cs.contains(1) shouldBe false
    cs.contains(0) shouldBe false

  "from iterable" in:
    val cs = Cset.from(List(1, 2, 3, 2, 1))
    cs.contains(1) shouldBe true
    cs.contains(2) shouldBe true
    cs.contains(3) shouldBe true
    cs.contains(4) shouldBe false

  "duplicate elements" in:
    val cs   = Cset(1, 2, 3, 2, 1)
    val list = cs.toList.sorted(using Ordering.Int)
    list shouldBe List(1, 2, 3)

  "union with empty" in:
    val cs1   = Cset(1, 2, 3)
    val cs2   = Cset.empty[Int]
    val list1 = (cs1 ++ cs2).toList.sorted
    list1 shouldBe List(1, 2, 3)
    val list2 = (cs2 ++ cs1).toList.sorted
    list2 shouldBe List(1, 2, 3)

  "intersect with empty" in:
    val cs1 = Cset(1, 2, 3)
    val cs2 = Cset.empty[Int]
    cs1.intersect(cs2).toList shouldBe List()
    cs2.intersect(cs1).toList shouldBe List()

  "diff with empty" in:
    val cs1  = Cset(1, 2, 3)
    val cs2  = Cset.empty[Int]
    val list = cs1.diff(cs2).toList.sorted
    list shouldBe List(1, 2, 3)
    cs2.diff(cs1).toList shouldBe List()

  "diff with self" in:
    val cs = Cset(1, 2, 3)
    cs.diff(cs).toList shouldBe List()

  "intersect with self" in:
    val cs   = Cset(1, 2, 3)
    val list = cs.intersect(cs).toList.sorted
    list shouldBe List(1, 2, 3)
  "no common elements" in:
    val cs1 = Cset(1, 2, 3)
    val cs2 = Cset(4, 5, 6)
    cs1.intersect(cs2).toList shouldBe List()
    val list = cs1.diff(cs2).toList.sorted
    list shouldBe List(1, 2, 3)

  "foldLeft" in:
    val cs = Cset(1, 2, 3, 4, 5)
    cs.foldLeft(0)(_ + _) shouldBe 15
  "map preserves uniqueness" in:
    val cs   = Cset(1, 2, 3, 4)
    val list = cs.map(_ % 2).toList.sorted
    list shouldBe List(0, 1)

  "with strings" in:
    val cs = Cset("apple", "banana", "cherry")
    cs.contains("apple") shouldBe true
    cs.contains("grape") shouldBe false
    (cs + "grape").contains("grape") shouldBe true

  "mixed operations" in:
    val cs1    = Cset(1, 2, 3, 4, 5)
    val cs2    = Cset(4, 5, 6, 7, 8)
    val cs3    = Cset(1, 3, 5, 7, 9)
    val result = (cs1.intersect(cs2)).diff(cs3)
    result.toList shouldBe List(4)

  "headOption & tail" in:
    val cs1 = Cset(10, 20, 30)
    cs1.headOption shouldBe Some(10)
    cs1.tail shouldBe Cset(20, 30)
    cs1.tail.tail shouldBe Cset(30)
    cs1.tail.tail.tail shouldBe C0
    val cs2 = Cset.empty[Int]
    cs2.headOption shouldBe None
    cs2.tail shouldBe C0
