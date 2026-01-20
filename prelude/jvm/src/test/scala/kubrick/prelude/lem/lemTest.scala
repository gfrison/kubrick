package kubrick.prelude.lem

import cats.Eval
import kubrick.prelude.lem.*
import kubrick.prelude.lem.all.{*, given}
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import scribe.*

import scala.collection.immutable.ArraySeq
class lemTest extends AsyncFreeSpec with Matchers:
  Logger.root.withMinimumLevel(Level.Debug).replace()
  "construction" in:
    Lem(1) shouldBe L1(1)

  "Bag construction with simple values" in:
    val bag = Bag(1, 2, 3)
    bag.shouldBe(a[B2[?]])

  "Bag construction with Set elements" in:
    val bag = Bag(Set(1, 2), Set(3, 4))
    bag.shouldBe(a[B2[?]])

  "Sek construction with simple values" in:
    val sek = Sek(1, 2, 3)
    sek.shouldBe(a[S2[?]])

  "Sek construction with Seq elements" in:
    val sek = Sek(List(1, 2), List(3, 4))
    sek.shouldBe(a[S2[?]])

  "Sek construction with Vector elements" in:
    val sek = Sek(Vector(1, 2), Vector(3, 4))
    sek.shouldBe(a[S2[?]])

  "Dict construction with simple tuples" in:
    val dict = Dict(("a", "b"), ("c", "d"))
    dict.shouldBe(a[D2[?]])

  "Dict construction with Map elements" in:
    val dict = Dict((Map("k1" -> "v1"), Map("k2" -> "v2")), (Map("k3" -> "v3"), Map("k4" -> "v4")))
    dict.shouldBe(a[D2[?]])
  "plus" - {
    "L1 + L1" in:
      val result = Lem(5) + 10
      result.shouldBe(a[Bag[?]])
      result shouldBe Bag(5, 10)
    "L1 ++ L1" in:
      val result = Lem(5) ++ Lem(10)
      result.shouldBe(a[Bag[?]])
      result shouldBe Bag(5, 10)
    "L1 + Pair" in:
      val result = Lem(5) + (3, 4)
      result shouldBe a[Bag[?]]
      result shouldBe a[Dict[?]]
    "Dict + Pair" in:
      val dict: Lem[String] = Dict("x" -> "y", "m" -> "n")
      val result            = dict + ("a" -> "b")
      result.shouldBe(a[D2[?]])

  }
