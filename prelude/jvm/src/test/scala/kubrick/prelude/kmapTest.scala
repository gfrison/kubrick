package kubrick.prelude

import kubrick.prelude.kmap.*
import kubrick.prelude.traits.*
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import scribe.*

class kmapTest extends AsyncFreeSpec with Matchers:
  Logger.root.withMinimumLevel(Level.Debug).replace()

  "construction" - {
    "should create Kmap with two elements" in:
      val map = Kmap(1 -> "a", 2 -> "b")
      map shouldBe a[Kmap[?, ?]]
      map.size shouldBe 2

    "should create Kmap with multiple elements" in:
      val map = Kmap(1 -> "a", 2 -> "b", 3 -> "c", 4 -> "d", 5 -> "e")
      map.size shouldBe 5

    "should update existing key" in:
      val map = Kmap(1 -> "a", 2 -> "b", 1 -> "c")
      map.size shouldBe 2
      map.get(1) shouldBe Some("c")

    "should create Kmap with string keys" in:
      val map = Kmap("a" -> 1, "b" -> 2, "c" -> 3)
      map.size shouldBe 3
      map.get("a") shouldBe Some(1)

    "should create empty Kmap" in:
      val map = Kmap[Int, String]()
      map.isEmpty shouldBe true
      map.size shouldBe 0

    "should create Kmap from Iterable" in:
      val map = Kmap.from(List(1 -> "a", 2 -> "b", 3 -> "c", 2 -> "d", 1 -> "e"))
      map.size shouldBe 3
      map.get(1) shouldBe Some("e")
      map.get(2) shouldBe Some("d")
      map.get(3) shouldBe Some("c")
  }

  "plus operation" - {
    "should add new key-value pair to map" in:
      val map = Kmap(1 -> "a", 2 -> "b")
      val result = map + (3 -> "c")
      result.size shouldBe 3
      result.get(3) shouldBe Some("c")

    "should update existing key with plus" in:
      val map = Kmap(1 -> "a", 2 -> "b", 3 -> "c")
      val result = map + (2 -> "updated")
      result.size shouldBe 3
      result.get(2) shouldBe Some("updated")

    "should maintain all existing elements when adding" in:
      val map = Kmap(1 -> "a", 2 -> "b")
      val result = map + (3 -> "c")
      result.get(1) shouldBe Some("a")
      result.get(2) shouldBe Some("b")
      result.get(3) shouldBe Some("c")

    "should preserve insertion order for new elements" in:
      val map = Kmap(1 -> "a", 2 -> "b")
      val result = map + (3 -> "c") + (4 -> "d")
      result.head._1 shouldBe 1
  }

  "contains operation" - {
    "should return true for existing keys" in:
      val map = Kmap(1 -> "a", 2 -> "b", 3 -> "c")
      map.contains((1 -> "a")) shouldBe true
      map.contains((2 -> "b")) shouldBe true
      map.contains((3 -> "c")) shouldBe true

    "should return false for non-existing keys" in:
      val map = Kmap(1 -> "a", 2 -> "b", 3 -> "c")
      map.contains((4 -> "d")) shouldBe false
      map.contains((0 -> "x")) shouldBe false

    "should work with different types" in:
      val map = Kmap("apple" -> 1, "banana" -> 2, "cherry" -> 3)
      map.contains(("apple" -> 1)) shouldBe true
      map.contains(("grape" -> 4)) shouldBe false
  }

  "head and tail operations" - {
    "should return head for non-empty map" in:
      val map = Kmap(1 -> "a", 2 -> "b", 3 -> "c")
      val h = map.head
      h shouldBe a[(Int, String)]
      h._1 shouldBe 1

    "should return headOption for non-empty map" in:
      val map = Kmap(1 -> "a", 2 -> "b", 3 -> "c")
      map.headOption shouldBe Some((1, "a"))

    "should return None for empty map headOption" in:
      val map = Kmap[Int, String]()
      map.headOption shouldBe None

    "should remove head with tail" in:
      val map = Kmap(1 -> "a", 2 -> "b", 3 -> "c")
      val tailMap = map.tail
      tailMap.size shouldBe 2
      tailMap.get(1) shouldBe None
      tailMap.get(2) shouldBe Some("b")
      tailMap.get(3) shouldBe Some("c")

    "should maintain correct size after multiple tail operations" in:
      val map = Kmap(1 -> "a", 2 -> "b", 3 -> "c", 4 -> "d", 5 -> "e")
      val tail1 = map.tail
      tail1.size shouldBe 4
      val tail2 = tail1.tail
      tail2.size shouldBe 3
  }

  "isEmpty and size" - {
    "should report correct size" in:
      Kmap(1 -> "a", 2 -> "b").size shouldBe 2
      Kmap(1 -> "a", 2 -> "b", 3 -> "c", 4 -> "d").size shouldBe 4

    "should be empty for new empty map" in:
      val map = Kmap[Int, String]()
      map.isEmpty shouldBe true
      map.size shouldBe 0

    "should not be empty for non-empty map" in:
      val map = Kmap(1 -> "a", 2 -> "b")
      map.isEmpty shouldBe false

    "should be empty after removing all elements" in:
      val map = Kmap(1 -> "a", 2 -> "b")
      val empty = map.tail.tail
      empty.isEmpty shouldBe true
      empty.size shouldBe 0
  }

  "get operation" - {
    "should retrieve values for existing keys" in:
      val map = Kmap(1 -> "a", 2 -> "b", 3 -> "c")
      map.get(1) shouldBe Some("a")
      map.get(2) shouldBe Some("b")
      map.get(3) shouldBe Some("c")

    "should return None for non-existing keys" in:
      val map = Kmap(1 -> "a", 2 -> "b")
      map.get(3) shouldBe None
      map.get(0) shouldBe None

    "should work with different key types" in:
      val map = Kmap("x" -> 1, "y" -> 2, "z" -> 3)
      map.get("x") shouldBe Some(1)
      map.get("w") shouldBe None
  }

  "unapply pattern matching" - {
    "should extract head and tail" in:
      val map = Kmap(1 -> "a", 2 -> "b", 3 -> "c")
      map match
        case (k, v) + rest =>
          k shouldBe 1
          v shouldBe "a"
          rest.size shouldBe 2
        case _ => fail("Should extract head and tail")

    "should work with empty Kmap" in:
      val map = Kmap[Int, String]()
      map match
        case (k, v) + rest => fail("Should not match empty Kmap")
        case _          => succeed
  }

  "IHeadTail trait integration" - {
    "should use head extension method" in:
      val map = Kmap(1 -> "a", 2 -> "b", 3 -> "c")
      val h = map.head
      h shouldBe a[(Int, String)]

    "should throw on head of empty map" in:
      val map = Kmap[Int, String]()
      assertThrows[NoSuchElementException]:
        map.head

    "should use isEmpty extension method" in:
      Kmap(1 -> "a", 2 -> "b").isEmpty shouldBe false
      Kmap[Int, String]().isEmpty shouldBe true

    "should use size extension method" in:
      Kmap(1 -> "a", 2 -> "b", 3 -> "c").size shouldBe 3
  }

  "IPlus trait integration" - {
    "should use plus method via + operator" in:
      val map = Kmap(1 -> "a", 2 -> "b")
      val result = map + (3 -> "c")
      result.size shouldBe 3
      result.get(3) shouldBe Some("c")

    "should use contains method via trait" in:
      val map = Kmap(1 -> "a", 2 -> "b", 3 -> "c")
      map.contains((2 -> "b")) shouldBe true
      map.contains((5 -> "e")) shouldBe false
  }

  "get and containsKey methods" - {
    "should get values by key" in:
      val map = Kmap(1 -> "a", 2 -> "b", 3 -> "c")
      map.get(1) shouldBe Some("a")
      map.get(2) shouldBe Some("b")
      map.get(3) shouldBe Some("c")

    "should return None for non-existing key" in:
      val map = Kmap(1 -> "a", 2 -> "b")
      map.get(99) shouldBe None
      map.get(0) shouldBe None

    "should check key existence with containsKey" in:
      val map = Kmap(1 -> "a", 2 -> "b", 3 -> "c")
      map.containsKey(1) shouldBe true
      map.containsKey(2) shouldBe true
      map.containsKey(3) shouldBe true

    "should return false for non-existing key with containsKey" in:
      val map = Kmap(1 -> "a", 2 -> "b")
      map.containsKey(99) shouldBe false
      map.containsKey(0) shouldBe false

    "should work with string keys" in:
      val map = Kmap("apple" -> 1, "banana" -> 2, "cherry" -> 3)
      map.get("apple") shouldBe Some(1)
      map.get("banana") shouldBe Some(2)
      map.containsKey("apple") shouldBe true
      map.containsKey("grape") shouldBe false

    "should work with updated values" in:
      val map = Kmap(1 -> "a", 2 -> "b")
      val updated = map + (1 -> "x")
      updated.get(1) shouldBe Some("x")
      updated.get(2) shouldBe Some("b")
      updated.containsKey(1) shouldBe true
  }

  "edge cases" - {
    "should handle single element" in:
      val map = Kmap(42 -> "answer")
      map.size shouldBe 1
      map.get(42) shouldBe Some("answer")
      map.head shouldBe ((42, "answer"))

    "should handle null as value" in:
      val map = Kmap[String, String]("a" -> null, "b" -> "value")
      map.size shouldBe 2
      map.get("a") shouldBe Some(null)

    "should handle large number of elements" in:
      val map = Kmap.from((1 to 1000).map(i => i -> s"value$i"))
      map.size shouldBe 1000
      map.get(500) shouldBe Some("value500")
      map.get(1001) shouldBe None
  }

  "insertion order" - {
    "should maintain insertion order (FIFO)" in:
      val map = Kmap(1 -> "a", 2 -> "b", 3 -> "c", 4 -> "d", 5 -> "e")
      map.head shouldBe ((1, "a"))
      map.tail.head shouldBe ((2, "b"))

    "should preserve order through from method" in:
      val map = Kmap.from(List(1 -> "a", 2 -> "b", 3 -> "c"))
      map.head shouldBe ((1, "a"))
  }

  "update behavior" - {
    "should update value when key exists" in:
      val map = Kmap(1 -> "a", 2 -> "b", 3 -> "c")
      val updated = map + (2 -> "updated")
      updated.size shouldBe 3
      updated.get(2) shouldBe Some("updated")

    "should maintain order when updating existing key" in:
      val map = Kmap(1 -> "a", 2 -> "b", 3 -> "c")
      val updated = map + (2 -> "updated")
      updated.head shouldBe ((1, "a"))
      updated.tail.head shouldBe ((2, "updated"))
  }

  "extractor plus (+)" in:
    val map = Kmap(1 -> "a", 2 -> "b", 3 -> "c")
    map match
      case head + tail =>
        head shouldBe ((1, "a"))
        tail.size shouldBe 2
        tail.get(1) shouldBe None
        tail.get(2) shouldBe Some("b")
      case _ => fail("Should match using + extractor")

  "Traverse type class" - {
    import cats.syntax.all.*
    
    "should fold left correctly" in:
      val map = Kmap(1 -> 10, 2 -> 20, 3 -> 30, 4 -> 40, 5 -> 50)
      map.foldLeft(0)(_ + _) shouldBe 150

    "should fold right correctly" in:
      val map = Kmap(1 -> 10, 2 -> 20, 3 -> 30)
      map.foldRight(cats.Eval.now(0))((a, b) => b.map(_ + a)).value shouldBe 60

    "should traverse with Option" in:
      val map = Kmap(1 -> 10, 2 -> 20, 3 -> 30)
      val result: Option[Kmap[Int, Int]] = map.traverse(n => if n > 0 then Some(n * 2) else None)
      result.map(_.get(1)) shouldBe Some(Some(20))
      result.map(_.get(2)) shouldBe Some(Some(40))
      result.map(_.get(3)) shouldBe Some(Some(60))

    "should traverse with Option and fail on condition" in:
      val map = Kmap(1 -> 10, 2 -> -20, 3 -> 30)
      val result: Option[Kmap[Int, Int]] = map.traverse(n => if n > 0 then Some(n * 2) else None)
      result shouldBe None

    "should traverse with List" in:
      val map = Kmap(1 -> 10, 2 -> 20)
      val result: List[Kmap[Int, Int]] = map.traverse(n => List(n, n * 10))
      result.length shouldBe 4

    "should map via Traverse" in:
      val map = Kmap(1 -> 10, 2 -> 20, 3 -> 30)
      val result = map.map(_ * 2)
      result.get(1) shouldBe Some(20)
      result.get(2) shouldBe Some(40)
      result.get(3) shouldBe Some(60)

    "should handle empty map traverse" in:
      val map = Kmap[Int, Int]()
      val result: Option[Kmap[Int, Int]] = map.traverse(n => Option(n * 2))
      result.map(_.isEmpty) shouldBe Some(true)

    "should sequence Option values" in:
      val map: Kmap[Int, Option[Int]] = Kmap.from(List(1 -> Some(10), 2 -> Some(20), 3 -> Some(30)))
      val result: Option[Kmap[Int, Int]] = map.traverse(opt => opt)
      result.map(_.get(1)) shouldBe Some(Some(10))
      result.map(_.get(2)) shouldBe Some(Some(20))

    "should fail to sequence when None present" in:
      val map: Kmap[Int, Option[Int]] = Kmap.from(List(1 -> Some(10), 2 -> None, 3 -> Some(30)))
      val result: Option[Kmap[Int, Int]] = map.traverse(opt => opt)
      result shouldBe None

    "should combine effects with traverse" in:
      var sum = 0
      val map = Kmap(1 -> 10, 2 -> 20, 3 -> 30)
      val result: Option[Kmap[Int, Int]] = map.traverse { n =>
        sum += n
        Option(n * 2)
      }
      sum shouldBe 60
      result.map(_.get(1)) shouldBe Some(Some(20))
  }

  "Bitraverse type class" - {
    import cats.syntax.all.*
    
    "should bifold left correctly" in:
      val map = Kmap(1 -> 10, 2 -> 20, 3 -> 30)
      val result = map.bifoldLeft(0)((acc, k) => acc + k, (acc, v) => acc + v)
      result shouldBe 66 // (1+2+3) + (10+20+30) = 6 + 60 = 66

    "should bifold right correctly" in:
      val map = Kmap(1 -> 10, 2 -> 20, 3 -> 30)
      val result = map.bifoldRight(cats.Eval.now(0))(
        (k, acc) => acc.map(_ + k),
        (v, acc) => acc.map(_ + v)
      ).value
      result shouldBe 66

    "should bitraverse with Option" in:
      val map = Kmap(1 -> 10, 2 -> 20, 3 -> 30)
      val result: Option[Kmap[Int, Int]] = map.bitraverse(
        k => if k > 0 then Some(k * 2) else None,
        v => if v > 0 then Some(v * 3) else None
      )
      result.map(_.get(4)) shouldBe Some(Some(60)) // key 2 becomes 4, value 20 becomes 60

    "should bitraverse and fail on key condition" in:
      val map = Kmap(1 -> 10, -2 -> 20, 3 -> 30)
      val result: Option[Kmap[Int, Int]] = map.bitraverse(
        k => if k > 0 then Some(k * 2) else None,
        v => Some(v * 3)
      )
      result shouldBe None

    "should bitraverse and fail on value condition" in:
      val map = Kmap(1 -> 10, 2 -> -20, 3 -> 30)
      val result: Option[Kmap[Int, Int]] = map.bitraverse(
        k => Some(k * 2),
        v => if v > 0 then Some(v * 3) else None
      )
      result shouldBe None

    "should bitraverse with List" in:
      val map = Kmap(1 -> 10, 2 -> 20)
      val result: List[Kmap[Int, Int]] = map.bitraverse(
        k => List(k, k + 100),
        v => List(v, v + 1000)
      )
      result.length shouldBe 16 // 2^4 combinations

    "should handle empty map bitraverse" in:
      val map = Kmap[Int, Int]()
      val result: Option[Kmap[Int, Int]] = map.bitraverse(
        k => Option(k * 2),
        v => Option(v * 3)
      )
      result.map(_.isEmpty) shouldBe Some(true)

    "should bimap keys and values" in:
      val map = Kmap(1 -> "a", 2 -> "b", 3 -> "c")
      val result = map.bimap(k => k * 10, v => v.toUpperCase)
      result.get(10) shouldBe Some("A")
      result.get(20) shouldBe Some("B")
      result.get(30) shouldBe Some("C")
  }
