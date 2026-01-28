module Test.Kube.Main where

import Prelude hiding (add)
import Effect (Effect)
import Kubrick.Kube (Kid(..), Kube, emptyKube, add)
import Control.Monad.State (evalState, runState)
import Data.List ((:), List(Nil))
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Kubrick.Lem (Lem(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.BuildGetTest as BuildGetTest

type Builder a = Kube a

emptyBuilder :: forall a. Ord a => Kube a
emptyBuilder = emptyKube

spec :: Spec Unit
spec = do
  describe "Builder - State Monad API" do

    describe "add function with State monad" do
      it "adds a single L1 value and returns its Kid" do
        let result = evalState (add (L1 42)) (Tuple (Kid 0) emptyBuilder)
        result `shouldEqual` (Kid 0)

      it "adds L1 and builds the structure" do
        let Tuple kid (Tuple _ builder) = runState (add (L1 42)) (Tuple (Kid 0) emptyBuilder)
        kid `shouldEqual` (Kid 0)
        Set.size builder.roots `shouldEqual` 1

      it "adds multiple documents and increments Kid" do
        let
          Tuple kids (Tuple _ builder) = runState
            ( do
                k1 <- add (L1 "a")
                k2 <- add (L1 "b")
                k3 <- add (L1 "c")
                pure [ k1, k2, k3 ]
            )
            (Tuple (Kid 0) emptyBuilder)
        kids `shouldEqual` [ Kid 0, Kid 1, Kid 2 ]
        Set.size builder.roots `shouldEqual` 3

      it "adds Sek and returns its Kid" do
        let
          sek = Sek (L1 1) (L1 2) ((L1 3) : Nil)
          Tuple kid (Tuple _ builder) = runState (add sek) (Tuple (Kid 0) emptyBuilder)
        kid `shouldEqual` (Kid 0)
        Set.size builder.roots `shouldEqual` 1

      it "allows composing multiple adds" do
        let
          Tuple result (Tuple _ builder) = runState
            ( do
                k1 <- add (L1 "first")
                k2 <- add (L1 "second")
                k3 <- add (L1 "third")
                pure (k1 + k2 + k3)
            )
            (Tuple (Kid 0) emptyBuilder)
        result `shouldEqual` (Kid 3) -- 0 + 1 + 2
        Set.size builder.roots `shouldEqual` 3

      it "can start with custom Kid counter" do
        let Tuple kid (Tuple _ _) = runState (add (L1 100)) (Tuple (Kid 42) emptyBuilder)
        kid `shouldEqual` (Kid 42)

      it "works with Bag" do
        let
          bag = Bag (L1 "a") (L1 "b") ((L1 "c") : Nil)
          Tuple kid (Tuple _ builder) = runState (add bag) (Tuple (Kid 0) emptyBuilder)
        kid `shouldEqual` (Kid 0)
        Set.size builder.roots `shouldEqual` 1
main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  spec
  BuildGetTest.spec