package kubrick.prelude.lem
import kubrick.prelude.lem.all.*
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import scribe.*

class d2Test extends AsyncFreeSpec with Matchers:
  Logger.root.withMinimumLevel(Level.Debug).replace()

  "D2 construction" - {
    "should create D2 with two key-value pairs" in:
      val dict = D2(("a", 1), ("b", 2))
      dict shouldBe a[D2[?]]
      dict.size shouldBe 2

    "should create D2 with multiple pairs" in:
      val dict = D2(("a", 1), ("b", 2), ("c", 3), ("d", 4))
      dict.size shouldBe 4

    "should create empty D2" in:
      val dict = D2.empty[Int]
      dict.isEmpty shouldBe true
      dict.size shouldBe 0

    "should handle duplicate keys by updating value" in:
      val dict = D2(("a", 1), ("b", 2), ("a", 3))
      dict.size shouldBe 2
      dict.get(L1("a")) shouldBe Some(L1(3))

    "should work with string key-value pairs" in:
      val dict = D2(("name", "Alice"), ("city", "NYC"))
      dict.size shouldBe 2
      dict.containsKey(L1("name")) shouldBe true
  }

  "D2 get operation" - {
    "should retrieve value for existing key" in:
      val dict = D2(("a", 1), ("b", 2), ("c", 3))
      dict.get(L1("a")) shouldBe Some(L1(1))
      dict.get(L1("b")) shouldBe Some(L1(2))
      dict.get(L1("c")) shouldBe Some(L1(3))

    "should return None for non-existing key" in:
      val dict = D2(("a", 1), ("b", 2))
      dict.get(L1("x")) shouldBe None
      dict.get(L1("z")) shouldBe None

    "should retrieve correct value after update" in:
      val dict = D2(("a", 1), ("b", 2))
      val updated = dict.updated(Pair("a", 10))
      updated.get(L1("a")) shouldBe Some(L1(10))
  }

  "D2 containsKey operation" - {
    "should return true for existing keys" in:
      val dict = D2(("a", 1), ("b", 2), ("c", 3))
      dict.containsKey(L1("a")) shouldBe true
      dict.containsKey(L1("b")) shouldBe true
      dict.containsKey(L1("c")) shouldBe true

    "should return false for non-existing keys" in:
      val dict = D2(("a", 1), ("b", 2))
      dict.containsKey(L1("x")) shouldBe false
      dict.containsKey(L1("z")) shouldBe false

    "should work correctly after updates" in:
      val dict = D2(("a", 1), ("b", 2))
      val updated = dict.updated(Pair("c", 3))
      updated.containsKey(L1("c")) shouldBe true
  }

  "D2 updated operation" - {
    "should add new key-value pair" in:
      val dict = D2(("a", 1), ("b", 2))
      val updated = dict.updated(Pair("c", 3))
      updated.size shouldBe 3
      updated.get(L1("c")) shouldBe Some(L1(3))

    "should update existing key with new value" in:
      val dict = D2(("a", 1), ("b", 2))
      val updated = dict.updated(Pair("a", 100))
      updated.size shouldBe 2
      updated.get(L1("a")) shouldBe Some(L1(100))
      updated.get(L1("b")) shouldBe Some(L1(2))

    "should maintain other pairs when updating" in:
      val dict = D2(("a", 1), ("b", 2), ("c", 3))
      val updated = dict.updated(Pair("b", 20))
      updated.size shouldBe 3
      updated.get(L1("a")) shouldBe Some(L1(1))
      updated.get(L1("b")) shouldBe Some(L1(20))
      updated.get(L1("c")) shouldBe Some(L1(3))

    "should work with empty dictionary" in:
      val dict = D2.empty[Int]
      val updated = dict.updated(Pair("first", 1))
      updated.size shouldBe 1
      updated.get(L1("first")) shouldBe Some(L1(1))
  }

  "D2 merge (++) operation" - {
    "should merge two D2 dictionaries" in:
      val dict1 = D2(("a", 1), ("b", 2))
      val dict2 = D2(("c", 3), ("d", 4))
      val merged = dict1 ++ dict2
      merged.size shouldBe 4
      merged.get(L1("a")) shouldBe Some(L1(1))
      merged.get(L1("d")) shouldBe Some(L1(4))

    "should override values for duplicate keys when merging" in:
      val dict1 = D2(("a", 1), ("b", 2))
      val dict2 = D2(("b", 20), ("c", 3))
      val merged = dict1 ++ dict2
      merged.size shouldBe 3
      merged.get(L1("a")) shouldBe Some(L1(1))
      merged.get(L1("b")) shouldBe Some(L1(20))
      merged.get(L1("c")) shouldBe Some(L1(3))

    "should handle merging with empty dictionary" in:
      val dict = D2(("a", 1), ("b", 2))
      val empty = D2.empty[Int]
      val merged1 = dict ++ empty
      val merged2 = empty ++ dict
      merged1.size shouldBe 2
      merged2.size shouldBe 2
  }

  "D2 head and tail operations" - {
    "should return head pair" in:
      val dict = D2(("a", 1), ("b", 2), ("c", 3))
      val head = dict.head
      head shouldBe a[Pair[?]]

    "should throw exception for head on empty dict" in:
      val dict = D2.empty[Int]
      assertThrows[UnsupportedOperationException]:
        dict.head

    "should remove head with tail" in:
      val dict = D2(("a", 1), ("b", 2), ("c", 3))
      val tailDict = dict.tail
      tailDict.size shouldBe 2

    "should throw exception for tail on empty dict" in:
      val dict = D2.empty[Int]
      assertThrows[UnsupportedOperationException]:
        dict.tail

    "should maintain correct size after multiple tail operations" in:
      val dict = D2(("a", 1), ("b", 2), ("c", 3), ("d", 4))
      val tail1 = dict.tail
      tail1.size shouldBe 3
      val tail2 = tail1.tail
      tail2.size shouldBe 2
  }

  "D2 isEmpty and size" - {
    "should report correct size" in:
      D2(("a", 1), ("b", 2)).size shouldBe 2
      D2(("a", 1), ("b", 2), ("c", 3), ("d", 4)).size shouldBe 4

    "should be empty for new empty dict" in:
      val dict = D2.empty[Int]
      dict.isEmpty shouldBe true
      dict.size shouldBe 0

    "should not be empty for non-empty dict" in:
      val dict = D2(("a", 1), ("b", 2))
      dict.isEmpty shouldBe false

    "should be empty after removing all elements" in:
      val dict = D2(("a", 1), ("b", 2))
      val empty = dict.tail.tail
      empty.isEmpty shouldBe true
      empty.size shouldBe 0
  }

  "D2 unapply pattern matching" - {
    "should match on non-empty dict" in:
      val dict = D2(("a", 1), ("b", 2))
      dict match
        case D2(head, tail) =>
          head shouldBe a[Pair[?]]
          tail shouldBe a[D2[?]]
          tail.size shouldBe 1
        case _ => fail("Should match non-empty dict")

    "should not match on empty dict" in:
      val dict = D2.empty[Int]
      dict match
        case D2(_, _) => fail("Should not match empty dict")
        case _        => succeed
  }

  "D2 with different types" - {
    "should work with integer keys and values" in:
      val dict = D2((1, 100), (2, 200))
      dict.size shouldBe 2
      dict.get(L1(1)) shouldBe Some(L1(100))

    "should work with mixed string pairs" in:
      val dict = D2(("key1", "value1"), ("key2", "value2"))
      dict.get(L1("key1")) shouldBe Some(L1("value1"))
      dict.get(L1("key2")) shouldBe Some(L1("value2"))

    "should handle complex update scenarios" in:
      val dict = D2(("x", 1), ("y", 2))
      val updated = dict.updated(Pair("x", 10)).updated(Pair("z", 3))
      updated.size shouldBe 3
      updated.get(L1("x")) shouldBe Some(L1(10))
      updated.get(L1("z")) shouldBe Some(L1(3))
  }
