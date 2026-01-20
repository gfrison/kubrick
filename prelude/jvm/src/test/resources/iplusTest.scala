package kubrick.prelude.lem

import kubrick.prelude.lem.*
import kubrick.prelude.lem.all.*
import kubrick.prelude.traits.*
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import scribe.*
import iplus.given

class iplusTest extends AsyncFreeSpec with Matchers:
  Logger.root.withMinimumLevel(Level.Debug).replace()
  
  "L1 + L1" - {
    "should create Bag with two elements" in:
      val result = L1(1) + L1(2)
      result shouldBe Bag(1, 2)
      result.shouldBe(a[Bag[?]])

    "should create Bag with strings" in:
      val result = L1("a") + L1("b")
      result shouldBe Bag("a", "b")
      result.shouldBe(a[B2[?]])

    "should create Bag with doubles" in:
      val result = L1(1.5) + L1(2.5)
      result.shouldBe(a[B2[?]])
  }

  "B2 + L1" - {
    "should add element to B2 bag" in:
      val b2 = B2(1, 2)
      val result = b2 + L1(3)
      result.shouldBe(a[B2[?]])

    "should add string element to B2" in:
      val b2 = B2("a", "b")
      val result = b2 + L1("c")
      result.shouldBe(a[B2[?]])

    "should handle boolean elements" in:
      val b2 = B2(true, false)
      val result = b2 + L1(true)
      result.shouldBe(a[B2[?]])
  }

  "B2 + B2" - {
    "should merge two B2 bags" in:
      val b1: Bag[Int] = B2(1, 2)
      val b2: Lem[Int] = B2(3, 4)
      val result = b1 + b2
      result.shouldBe(a[Bag[?]])

    "should merge string bags" in:
      val b1: Bag[String] = B2("x", "y")
      val b2: Lem[String] = B2("z", "w")
      val result = b1 + b2
      result.shouldBe(a[Bag[?]])
  }
  "sek + pair" in:
    val res = (Sek[Int](1, 2) + L2[Int](L1(3), L1(4))) 
    // res shouldBe Sekdict( Sek(1,2), Pair(3,4))
    succeed