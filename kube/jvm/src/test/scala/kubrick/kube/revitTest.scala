package kubrick.kube

import kubrick.kube.all.*
import kubrick.prelude.all.{*, given}
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import scribe.*
class revitTest extends AsyncFreeSpec with Matchers:
  Logger.root.withMinimumLevel(Level.Debug).replace()
