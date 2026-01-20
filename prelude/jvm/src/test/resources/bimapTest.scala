package kubrick.prelude
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

import all.{*, given}
class bimapTest extends AsyncFreeSpec with Matchers:
  "gets" in:
    val bim = Bimap("a" -> 1, "b" -> 2)
    bim.getValue("a") shouldBe Some(1)
    bim.getKey(2) shouldBe Some("b")
  "putIfAbsent" in:
    val bim = Bi0 + ("x" -> 42)
    bim
      .putIfAbsent(
        "x",
        () => {
          throw new Exception("should not be called")
          100
        }
      )
      .getValue("x") shouldBe Some(42)
  "size" in:
    val bim = Bimap("a" -> 1, "b" -> 2, "c" -> 3)
    bim.size shouldBe 3
