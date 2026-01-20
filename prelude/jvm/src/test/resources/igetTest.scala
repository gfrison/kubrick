package kubrick.prelude.lem

import kubrick.prelude.lem.*
import kubrick.prelude.lem.all.*
import kubrick.prelude.traits.*
import kubrick.prelude.kmap.Kmap
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import scribe.*
import iget.given
import language.postfixOps

class igetTest extends AsyncFreeSpec with Matchers:
  Logger.root.withMinimumLevel(Level.Debug).replace()

  "D2 IGet - get existing key" in:
    val dict = D2(("key1", "value1"), ("key2", "value2"), ("key3", "value3"))
    dict.get(L1("key1")) should contain (L1("value1"))

  "D2 IGet - get non-existing key" in:
    val dict = D2(("key1", "value1"), ("key2", "value2"))
    val result = dict.get(L1("key3"))
    result shouldBe empty

  "D2 IGet - containsKey for existing key" in:
    val dict = D2(("a", "b"), ("c", "d"))
    dict.containsKey(L1("a")) shouldBe true
    dict.containsKey(L1("c")) shouldBe true

  "D2 IGet - containsKey for non-existing key" in:
    val dict = D2(("a", "b"), ("c", "d"))
    dict.containsKey(L1("e")) shouldBe false

  "D2 IGet - get with simple key" in:
    val dict = D2(("x", "y"), ("z", "w"))
    dict.get(L1("x")) should contain (L1("y"))

  "D2 IGet - empty dictionary" in:
    val dict = D2.empty[String]
    val result = dict.get(L1("anything"))
    result shouldBe empty

  "D2 IGet - containsKey on empty dictionary" in:
    val dict = D2.empty[String]
    dict.containsKey(L1("anything")) shouldBe false

  "D2 IGet - multiple lookups" in:
    val dict = D2(("1", "one"), ("2", "two"), ("3", "three"))
    dict.get(L1("1")) shouldBe Some(L1("one"))
    dict.get(L1("2")) shouldBe Some(L1("two"))
    dict.get(L1("3")) shouldBe Some(L1("three"))
    dict.get(L1("4")) shouldBe None

  "Pair IGet - get with matching key" in:
    val pair = Pair("key", "value")
    pair.get(L1("key"): Lem[String]) should contain (L1("value"))

  "Pair IGet - get with non-matching key" in:
    val pair = Pair("key", "value")
    val result = pair.get(L1("otherkey"): Lem[String])
    result shouldBe empty

  "Pair IGet - containsKey with matching key" in:
    val pair = Pair("mykey", "myvalue")
    pair.containsKey(L1("mykey")) shouldBe true

  "Pair IGet - containsKey with non-matching key" in:
    val pair = Pair("mykey", "myvalue")
    pair.containsKey(L1("wrongkey")) shouldBe false

  "Pair IGet - get using simple values" in:
    val pair = Pair("alpha", "beta")
    pair.get(L1("alpha")) should contain (L1("beta"))

  "Pair IGet - containsKey using simple values" in:
    val pair = Pair("x", "y")
    pair.containsKey(L1("x")) shouldBe true
    pair.containsKey(L1("y")) shouldBe false // y is the value, not the key
    pair.containsKey(L1("z")) shouldBe false

  "Pair IGet - numeric keys and values" in:
    val pair = Pair(42, 100)
    pair.get(L1(42)) shouldBe Some(L1(100))
    pair.get(L1(100)) shouldBe None
    pair.containsKey(L1(42)) shouldBe true
    pair.containsKey(L1(100)) shouldBe false

  "IGet integration - D2 with Bag keys" in:
    // Create D2 with Bag keys by constructing directly
    val key1: Lem[Any] = B2(1, 2)
    val key2: Lem[Any] = B2(3, 4)
    val key3: Lem[Any] = B2(5, 6)
    val val1: Lem[Any] = L1("first")
    val val2: Lem[Any] = L1("second")
    
    val dict = new D2(Kmap.empty[Lem[Any], Lem[Any]] + (key1 -> val1) + (key2 -> val2))
    
    dict.get(key1) shouldBe Some(val1)
    dict.get(key2) shouldBe Some(val2)
    dict.get(key3) shouldBe None

  "IGet integration - Pair with nested Lem values" in:
    val nestedKey: Lem[String] = B2("a", "b")
    val nestedValue = B2("x", "y")
    val pair = L2(nestedKey, nestedValue)
    
    pair.get(nestedKey) shouldBe Some(nestedValue)
    pair.get(B2("c", "d")) shouldBe None
