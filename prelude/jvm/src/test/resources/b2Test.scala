package kubrick.prelude.lem
import kubrick.prelude.lem.all.*
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import scribe.*

class b2Test extends AsyncFreeSpec with Matchers:
  Logger.root.withMinimumLevel(Level.Debug).replace()

  "B2 construction" - {
    "should create B2 with two elements" in:
      val bag = B2(1, 2)
      bag shouldBe a[B2[?]]
      bag.size shouldBe 2

    "should create B2 with multiple elements" in:
      val bag = B2(1, 2, 3, 4, 5)
      bag.size shouldBe 5

    "should deduplicate elements" in:
      val bag = B2(L1(1), L1(2), L1(1), L1(3))
      bag.size shouldBe 3
      bag.contains(L1(1)) shouldBe true
      bag.contains(L1(2)) shouldBe true
      bag.contains(L1(3)) shouldBe true

    "should create B2 with string elements" in:
      val bag = B2("a", "b", "c")
      bag.size shouldBe 3
      bag.contains(L1("a")) shouldBe true
  }

  "B2 add operation" - {
    "should add new element to bag" in:
      val bag = B2(1, 2)
      val result = bag.add(L1(3))
      result.size shouldBe 3
      result.contains(L1(3)) shouldBe true

    "should not add duplicate element" in:
      val bag = B2(1, 2, 3)
      val result = bag.add(L1(2))
      result.size shouldBe 3

    "should maintain all existing elements when adding" in:
      val bag = B2(1, 2)
      val result = bag.add(L1(3))
      result.contains(L1(1)) shouldBe true
      result.contains(L1(2)) shouldBe true
      result.contains(L1(3)) shouldBe true
  }

  "B2 merge (++) operation" - {
    "should merge two B2 bags" in:
      val bag1 = B2(1, 2)
      val bag2 = B2(3, 4)
      val result = bag1 ++ bag2
      result.size shouldBe 4

    "should deduplicate when merging" in:
      val bag1 = B2(1, 2, 3)
      val bag2 = B2(2, 3, 4)
      val result = bag1 ++ bag2
      result.size shouldBe 4
      result.contains(L1(1)) shouldBe true
      result.contains(L1(4)) shouldBe true

    "should handle empty merge" in:
      val bag1 = B2(1, 2)
      val bag2 = B2(1, 2)
      val result = bag1 ++ bag2
      result.size shouldBe 2
  }

  "B2 contains operation" - {
    "should return true for existing elements" in:
      val bag = B2(1, 2, 3)
      bag.contains(L1(1)) shouldBe true
      bag.contains(L1(2)) shouldBe true
      bag.contains(L1(3)) shouldBe true

    "should return false for non-existing elements" in:
      val bag = B2(1, 2, 3)
      bag.contains(L1(4)) shouldBe false
      bag.contains(L1(0)) shouldBe false
  }

  "B2 head and tail operations" - {
    "should return head element" in:
      val bag = B2(1, 2, 3)
      bag.getHead shouldBe a[Lem[?]]

    "should remove head with tail" in:
      val bag = B2(1, 2, 3)
      val tailBag = bag.getTail
      tailBag.size shouldBe 2

    "should maintain correct size after multiple tail operations" in:
      val bag = B2(1, 2, 3, 4, 5)
      val tail1 = bag.getTail
      tail1.size shouldBe 4
      val tail2 = tail1.getTail
      tail2.size shouldBe 3
  }

  "B2 isEmpty and size" - {
    "should report correct size" in:
      B2(1, 2).size shouldBe 2
      B2(1, 2, 3, 4).size shouldBe 4

    "should not be empty for non-empty bag" in:
      val bag = B2(1, 2)
      bag.isEmpty shouldBe false

    "should be empty after removing all elements" in:
      val bag = B2(1, 2)
      val empty = bag.getTail.getTail
      empty.isEmpty shouldBe true
      empty.size shouldBe 0
  }

  "B2 with different types" - {
    "should work with strings" in:
      val bag = B2("apple", "banana", "cherry")
      bag.size shouldBe 3
      bag.contains(L1("apple")) shouldBe true

    "should work with mixed numeric types" in:
      val bag = B2(1, 2, 3)
      bag.add(L1(4)).size shouldBe 4
  }