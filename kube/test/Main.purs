module Test.Kube.Main where

import Prelude hiding (add)
import Effect (Effect)
import Kubrick.Kube (Kid(..), Kube, emptyKube, addM)
import Control.Monad.State (evalState, runState)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Kubrick.Types (Raw(..))
import Kubrick.Lem (Lem(..), (+:), (<+>))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.BuildGetTest as BuildGetTest
import Test.Kube.BuilderTest as BuilderTest
import Test.Kube.MatcherTest as MatcherTest

type Builder = Kube

emptyBuilder :: Kube
emptyBuilder = emptyKube

spec :: Spec Unit
spec = do
  describe "Builder - State Monad API" do

    describe "add function with State monad" do
      it "adds a single L1 value and returns its Kid" do
        let result = evalState (addM (L1 (Ri 42))) (Tuple (Kid 0) emptyBuilder)
        result `shouldEqual` (Kid 0)

      it "adds L1 and builds the structure" do
        let Tuple kid (Tuple _ builder) = runState (addM (L1 (Ri 42))) (Tuple (Kid 0) emptyBuilder)
        kid `shouldEqual` (Kid 0)
        Set.size builder.roots `shouldEqual` 1

      it "adds multiple documents and increments Kid" do
        let
          Tuple kids (Tuple _ builder) = runState
            ( do
                k1 <- addM (L1 (Rs "a"))
                k2 <- addM (L1 (Rs "b"))
                k3 <- addM (L1 (Rs "c"))
                pure [ k1, k2, k3 ]
            )
            (Tuple (Kid 0) emptyBuilder)
        kids `shouldEqual` [ Kid 0, Kid 1, Kid 2 ]
        Set.size builder.roots `shouldEqual` 3

      it "adds Sek and returns its Kid" do
        let
          sek :: Lem Raw
          sek = (Ri 1) +:   ((Ri 2) +:   ((Ri 3) +:   L0))
          Tuple kid (Tuple _ builder) = runState (addM sek) (Tuple (Kid 0) emptyBuilder)
        kid `shouldEqual` (Kid 1)
        Set.size builder.roots `shouldEqual` 1

      it "allows composing multiple adds" do
        let
          Tuple result (Tuple _ builder) = runState
            ( do
                k1 <- addM (L1 (Rs "first"))
                k2 <- addM (L1 (Rs "second"))
                k3 <- addM (L1 (Rs "third"))
                pure (k1 + k2 + k3)
            )
            (Tuple (Kid 0) emptyBuilder)
        result `shouldEqual` (Kid 3) -- 0 + 1 + 2
        Set.size builder.roots `shouldEqual` 3

      it "can start with custom Kid counter" do
        let Tuple kid (Tuple _ _) = runState (addM (L1 (Ri 100))) (Tuple (Kid 42) emptyBuilder)
        kid `shouldEqual` (Kid 42)

      it "works with Bag" do
        let
          bag = L1 (Rs "a") <+> L1 (Rs "b") <+> L1 (Rs "c")
          Tuple kid (Tuple _ builder) = runState (addM bag) (Tuple (Kid 0) emptyBuilder)
        kid `shouldEqual` (Kid 0)
        Set.size builder.roots `shouldEqual` 1
main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  spec
  BuildGetTest.spec
  BuilderTest.spec
  MatcherTest.spec