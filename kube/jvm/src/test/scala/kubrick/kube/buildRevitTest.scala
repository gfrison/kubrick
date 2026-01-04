package kubrick.kube
import kubrick.kube.all.*
import kubrick.prelude.all.{*, given}
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import scribe.*
class buildRevitTest extends AsyncFreeSpec with Matchers:
  Logger.root.withMinimumLevel(Level.Debug).replace()
  "sek1" in:
    single(Sek("a", "b", "c"))
  "sek2" in:
    single(Sek("a", "b") + "c")
  "pair in sek" in:
    single(Sek("a", "b") + ("c" --> "d"))
  "pair" in:
    single("a" --> "b")
  "choice" in:
    single(Choice("a", "b", "c"))
  "sek choice" in:
    single(Sek("a","b") + Choice("c","d"))
  def single(doc: Doc) =
    debug(s"doc: $doc")
    val k = K0 + doc
    k.get(k.roots.head) should contain(doc)
