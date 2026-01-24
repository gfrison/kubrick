module Test.Main where

import Prelude
import Effect (Effect)
import Test.Lem as LemTests
import Test.Lem.Constructor as ConstructorTests
import Test.Lem.Integrity as IntegrityTests
import Test.Lem.Show as ShowTests
import Test.Lem.Traversable as TraversableTests
import Test.Lem.Uniqueness as UniquenessTests
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  LemTests.spec
  ShowTests.spec
  TraversableTests.spec
  UniquenessTests.spec
  IntegrityTests.spec
  ConstructorTests.spec
