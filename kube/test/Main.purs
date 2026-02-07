module Test.Kube.Main where

import Prelude
import Effect (Effect)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.BuildGetTest as BuildGetTest
import Test.Kube.BuilderTest as BuilderTest
import Test.Kube.MatcherTest as MatcherTest
import Test.Kube.MonadTest as MonadTest
import Test.Kube.SearcherTest as SearcherTest

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  MonadTest.spec
  BuildGetTest.spec
  BuilderTest.spec
  MatcherTest.spec
  SearcherTest.spec