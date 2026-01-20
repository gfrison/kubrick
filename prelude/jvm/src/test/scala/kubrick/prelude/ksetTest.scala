package kubrick.prelude

import kubrick.prelude.kset.{*,given}
import kubrick.prelude.traits.*
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import scribe.*

class ksetTest extends AsyncFreeSpec with Matchers:
  Logger.root.withMinimumLevel(Level.Debug).replace()

  "construction" - {
    "should create Kset with two elements" in:
      val set = Kset(1, 2)
      set shouldBe a[Kset[?]]
      set.size shouldBe 2

    "should create Kset with multiple elements" in:
      val set = Kset(1, 2, 3, 4, 5)
      set.size shouldBe 5

    "should deduplicate elements" in:
      val set = Kset(1, 2, 1, 3, 2)
      set.size shouldBe 3
      set.contains(1) shouldBe true
      set.contains(2) shouldBe true
      set.contains(3) shouldBe true

    "should create Kset with string elements" in:
      val set = Kset("a", "b", "c")
      set.size shouldBe 3
      set.contains("a") shouldBe true

    "should create empty Kset" in:
      val set = Kset[Int]()
      set.isEmpty shouldBe true
      set.size shouldBe 0

    "should create Kset from Iterable" in:
      val set = Kset.from(List(1, 2, 3, 2, 1))
      set.size shouldBe 3
      set.contains(1) shouldBe true
      set.contains(2) shouldBe true
      set.contains(3) shouldBe true
  }

  "plus operation" - {
    "should add new element to set" in:
      val set    = Kset(1, 2)
      val result = set + 3
      result.size shouldBe 3
      result.contains(3) shouldBe true

    "should not add duplicate element" in:
      val set    = Kset(1, 2, 3)
      val result = set + 2
      result.size shouldBe 3

    "should maintain all existing elements when adding" in:
      val set    = Kset(1, 2)
      val result = set + 3
      result.contains(1) shouldBe true
      result.contains(2) shouldBe true
      result.contains(3) shouldBe true

    "should preserve insertion order for new elements" in:
      val set    = Kset(1, 2)
      val result = set + 3 + 4
      result.headOption shouldBe Some(4)
  }
  "++" in:
    val result = Kset(1, 2) ++ Kset(3, 4, 2)
    result.size shouldBe 4
    result.contains(1) shouldBe true
    result.contains(2) shouldBe true
    result.contains(3) shouldBe true
    result.contains(4) shouldBe true

  "contains operation" - {
    "should return true for existing elements" in:
      val set = Kset(1, 2, 3)
      set.contains(1) shouldBe true
      set.contains(2) shouldBe true
      set.contains(3) shouldBe true

    "should return false for non-existing elements" in:
      val set = Kset(1, 2, 3)
      set.contains(4) shouldBe false
      set.contains(0) shouldBe false

    "should work with different types" in:
      val set = Kset("apple", "banana", "cherry")
      set.contains("apple") shouldBe true
      set.contains("grape") shouldBe false
  }

  "head and tail operations" - {
    "should return headOption for non-empty set" in:
      val set = Kset(1, 2, 3)
      set.headOption shouldBe defined
      set.headOption.get shouldBe a[Int]

    "should return None for empty set headOption" in:
      val set = Kset[Int]()
      set.headOption shouldBe None

    "should remove head with tail" in:
      val set     = Kset(1, 2, 3)
      val tailSet = set.tail
      tailSet.size shouldBe 2
      // Head is the last added element due to LIFO insertion order
      tailSet.contains(1) shouldBe true
      tailSet.contains(2) shouldBe true

    "should maintain correct size after multiple tail operations" in:
      val set   = Kset(1, 2, 3, 4, 5)
      val tail1 = set.tail
      tail1.size shouldBe 4
      val tail2 = tail1.tail
      tail2.size shouldBe 3
  }

  "isEmpty and size" - {
    "should report correct size" in:
      Kset(1, 2).size shouldBe 2
      Kset(1, 2, 3, 4).size shouldBe 4

    "should be empty for new empty set" in:
      val set = Kset[Int]()
      set.isEmpty shouldBe true
      set.size shouldBe 0

    "should not be empty for non-empty set" in:
      val set = Kset(1, 2)
      set.isEmpty shouldBe false

    "should be empty after removing all elements" in:
      val set   = Kset(1, 2)
      val empty = set.tail.tail
      empty.isEmpty shouldBe true
      empty.size shouldBe 0
  }

  "with different types" - {
    "should work with strings" in:
      val set = Kset("apple", "banana", "cherry")
      set.size shouldBe 3
      set.contains("apple") shouldBe true
      set.contains("grape") shouldBe false

    "should work with mixed numeric types (widening)" in:
      val set: Kset[Double | Int] = Kset(1, 2, 3)
      val result         = set + (4.0)
      result.size shouldBe 4

    "should work with case classes" in:
      case class Person(name: String, age: Int)
      val set = Kset(Person("Alice", 30), Person("Bob", 25))
      set.size shouldBe 2
      set.contains(Person("Alice", 30)) shouldBe true
  }

  "unapply pattern matching" - {
    "should extract elements as a list" in:
      val set = Kset(1, 2, 3)
      set match
        case Kset(elems) =>
          // Elements are extracted as a list in reverse insertion order
          elems.toSet shouldBe Set(1, 2, 3)
        case _ => fail("Should extract elements")

    "should work with empty Kset" in:
      val set = Kset[Int]()
      set match
        case Kset(elems) =>
          elems.isEmpty shouldBe true
        case _ => fail("Should match empty Kset")
  }

  "IHeadTail trait integration" - {
    "should use head extension method" in:
      val set = Kset(1, 2, 3)
      set.head shouldBe an[Int]

    "should throw on head of empty set" in:
      val set = Kset[Int]()
      assertThrows[NoSuchElementException]:
        set.head

    "should use isEmpty extension method" in:
      Kset(1, 2).isEmpty shouldBe false
      Kset[Int]().isEmpty shouldBe true

    "should use size extension method" in:
      Kset(1, 2, 3).size shouldBe 3
  }

  "IPlus trait integration" - {
    "should use plus method" in:
      val set    = Kset(1, 2)
      val result = set + 3
      result.size shouldBe 3
      result.contains(3) shouldBe true

    "should use contains method via trait" in:
      val set = Kset(1, 2, 3)
      set.contains(2) shouldBe true
      set.contains(5) shouldBe false
  }

  "edge cases" - {
    "should handle single element" in:
      val set = Kset(42)
      set.size shouldBe 1
      set.contains(42) shouldBe true
      set.headOption shouldBe Some(42)

    "should handle null as element" in:
      val set = Kset[String](null, "a", "b")
      set.size shouldBe 3
      set.contains(null.asInstanceOf[String]) shouldBe true

    "should handle large number of elements" in:
      val set = Kset.from(1 to 1000)
      set.size shouldBe 1000
      set.contains(500) shouldBe true
      set.contains(1001) shouldBe false
  }

  "insertion order" - {
    "should maintain LIFO order (last in, first out)" in:
      val set = Kset(1, 2, 3, 4, 5)
      set.headOption shouldBe Some(5)
      set.tail.headOption shouldBe Some(4)

    "should preserve order through from method" in:
      val set = Kset.from(List(1, 2, 3))
      // Due to LIFO, the last element added (3) should be head
      set.headOption shouldBe Some(3)
  }

  "deduplication behavior" - {
    "should deduplicate during construction" in:
      val set = Kset(1, 2, 1, 3, 2, 4, 3)
      set.size shouldBe 4

    "should deduplicate when adding elements" in:
      val set  = Kset(1, 2, 3)
      val same = set + 1 + 2 + 3
      same.size shouldBe 3

    "should maintain first insertion when duplicates are added" in:
      val set     = Kset(1, 2, 3)
      val withDup = set + 2
      // Size should remain the same since 2 already exists
      withDup.size shouldBe 3
      withDup.contains(2) shouldBe true
  }
  "extractor plus (+)" in:
    val set = Kset(1, 2, 3)
    set match
      case head + tail =>
        head shouldBe 3
        tail.size shouldBe 2
        tail.contains(1) shouldBe true
        tail.contains(2) shouldBe true
      case _ => fail("Should match using + extractor")

  "Traverse type class" - {
    import cats.syntax.all.*
    
    "should fold left correctly" in:
      val set = Kset(1, 2, 3, 4, 5)
      set.foldLeft(0)(_ + _) shouldBe 15

    "should fold right correctly" in:
      val set = Kset(1, 2, 3)
      set.foldRight(cats.Eval.now(0))((a, b) => b.map(_ + a)).value shouldBe 6

    "should traverse with Option" in:
      val set = Kset(1, 2, 3)
      val result: Option[Kset[Int]] = set.traverse(n => if n > 0 then Some(n * 2) else None)
      result shouldBe Some(Kset(2, 4, 6))

    "should traverse with Option and fail on condition" in:
      val set = Kset(1, -2, 3)
      val result: Option[Kset[Int]] = set.traverse(n => if n > 0 then Some(n * 2) else None)
      result shouldBe None

    "should traverse with List" in:
      val set = Kset(1, 2)
      val result: List[Kset[Int]] = set.traverse(n => List(n, n * 10))
      // Should produce all combinations
      result.length shouldBe 4

    "should map via Traverse" in:
      val set = Kset(1, 2, 3)
      val result = set.map(_ * 2)
      result shouldBe Kset(2, 4, 6)

    "should preserve uniqueness after traverse" in:
      val set = Kset(1, 2, 3, 4)
      val result: Option[Kset[Int]] = set.traverse(n => Option(n % 2))
      result.map(_.size) shouldBe Some(2)

    "should handle empty set traverse" in:
      val set = Kset[Int]()
      val result: Option[Kset[Int]] = set.traverse(n => Option(n * 2))
      result.map(_.isEmpty) shouldBe Some(true)

    "should sequence Option values" in:
      val set: Kset[Option[Int]] = Kset.from(List(Some(1), Some(2), Some(3)))
      val result: Option[Kset[Int]] = set.traverse(opt => opt)
      result shouldBe Some(Kset(1, 2, 3))

    "should fail to sequence when None present" in:
      val set: Kset[Option[Int]] = Kset.from(List(Some(1), None, Some(3)))
      val result: Option[Kset[Int]] = set.traverse(opt => opt)
      result shouldBe None

    "should combine effects with traverse" in:
      var sum = 0
      val set = Kset(1, 2, 3)
      val result: Option[Kset[Int]] = set.traverse { n =>
        sum += n
        Option(n * 2)
      }
      sum shouldBe 6
      result shouldBe Some(Kset(2, 4, 6))
  }
