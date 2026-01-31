module Test.BuildGetTest where

import Prelude hiding (add)

import Kubrick.Kube (Kid(..), Kube, emptyKube, addM)
import Kubrick.Getter as Getter
import Control.Monad.State (runState)
import Data.List ((:), List(Nil))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Kubrick.Types (Raw(..))
import Kubrick.Lem (Lem(..), (+:), (<+), (\/), Bag1(..), Dict1(..), lem)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "BuildGet - Round-trip tests (build with add, retrieve with get)" do

    it "reconstructs structure built with add" do
      -- Integration test: build with add, then get back
      let
        original = (Ri 1) +: ((Ri 2) +: ((Ri 3) +: L0))
        Tuple kid (Tuple _ kube) = runState (addM original) (Tuple (Kid 0) (emptyKube))
        result = Getter.get kube kid
      result `shouldEqual` (Just original)

    it "reconstructs Pair built with add" do
      let
        original = lem (((Ri 1) /\ (Ri 2)) : Nil)  -- Single Pair is ok
        Tuple kid (Tuple _ kube) = runState (addM original) (Tuple (Kid 0) (emptyKube))
        result = Getter.get kube kid
      result `shouldEqual` (Just original)

    it "reconstructs Choice built with add" do
      let
        original = L1 (Ri 1) \/ L1 (Ri 2) \/ L1 (Ri 3)
        Tuple kid (Tuple _ kube) = runState (addM original) (Tuple (Kid 0) (emptyKube))
        result = Getter.get kube kid
        -- Getter reconstructs as flat Choice with list
        expected = Choice (L1 (Ri 1)) (L1 (Ri 2)) (L1 (Ri 3) : Nil)
      result `shouldEqual` (Just expected)

    it "reconstructs simple Dict (Pair form)" do
      let
        -- Single key-value pair becomes Pair
        original = lem (((Ri 100) /\ (Ri 200)) : Nil)  -- Single Pair is ok
        Tuple kid (Tuple _ kube) = runState (addM original) (Tuple (Kid 0) (emptyKube))
        result = Getter.get kube kid
      result `shouldEqual` (Just original)

    it "reconstructs Sek with L1 elements" do
      let
        original = (Ri 100) +: ((Ri 200) +: ((Ri 300) +: ((Ri 400) +: L0)))
        Tuple kid (Tuple _ kube) = runState (addM original) (Tuple (Kid 0) (emptyKube))
        result = Getter.get kube kid
      result `shouldEqual` (Just original)

    it "reconstructs Dict as Bagdict (Dict doesn't preserve structure)" do
      let
        -- Dict creates a dictionary, but round-trip changes it to Bagdict
        original = lem (((Ri 1000) /\ (Ri 1100)) : Nil) <+ ((Ri 2000) /\ (Ri 2200))
        Tuple kid (Tuple _ kube) = runState (addM original) (Tuple (Kid 0) (emptyKube))
        result = Getter.get kube kid
        -- Expect Bagdict, not Dict - this is what Getter reconstructs
        expected = Bagdict (B1 (Pair (L1 (Ri 2000)) (L1 (Ri 2200)))) (D1 (L1 (Ri 1000)) (L1 (Ri 1100)))
      result `shouldEqual` (Just expected)

    it "reconstructs Choice with simple elements" do
      let
        original = L1 (Ri 100) \/ L1 (Ri 200) \/ L1 (Ri 300) \/ L1 (Ri 400)
        Tuple kid (Tuple _ kube) = runState (addM original) (Tuple (Kid 0) (emptyKube))
        result = Getter.get kube kid
        -- Getter reconstructs as flat Choice with list
        expected = Choice (L1 (Ri 100)) (L1 (Ri 200)) (L1 (Ri 300) : L1 (Ri 400) : Nil)
      result `shouldEqual` (Just expected)

    it "round-trip preserves L1 values" do
      let
        original = L1 (Ri 12345)
        Tuple kid (Tuple _ kube) = runState (addM original) (Tuple (Kid 0) (emptyKube))
        result = Getter.get kube kid
      result `shouldEqual` (Just original)

    it "round-trip preserves simple Pair" do
      let
        original = lem (((Ri 5000) /\ (Ri 6000)) : Nil)  -- Single Pair is ok
        Tuple kid (Tuple _ kube) = runState (addM original) (Tuple (Kid 0) (emptyKube))
        result = Getter.get kube kid
      result `shouldEqual` (Just original)

    it "round-trip preserves simple Sek" do
      let
        original = (Ri 9000) +: ((Ri 9001) +: ((Ri 9002) +: L0))
        Tuple kid (Tuple _ kube) = runState (addM original) (Tuple (Kid 0) (emptyKube))
        result = Getter.get kube kid
      result `shouldEqual` (Just original)

    it "round-trip preserves simple Choice" do
      let
        original = L1 (Ri 7000) \/ L1 (Ri 7001) \/ L1 (Ri 7002)
        Tuple kid (Tuple _ kube) = runState (addM original) (Tuple (Kid 0) (emptyKube))
        result = Getter.get kube kid
        -- Getter reconstructs as flat Choice with list
        expected = Choice (L1 (Ri 7000)) (L1 (Ri 7001)) (L1 (Ri 7002) : Nil)
      result `shouldEqual` (Just expected)

    it "Dict with 3+ pairs becomes Bag structure on round-trip" do
      let
        -- Dict with 3+ pairs doesn't preserve as Dict
        original = lem (((Ri 10) /\ (Ri 20)) : Nil) <+ ((Ri 30) /\ (Ri 40)) <+ ((Ri 50) /\ (Ri 60))
        Tuple kid (Tuple _ kube) = runState (addM original) (Tuple (Kid 0) (emptyKube))
        result = Getter.get kube kid
        -- Expect Bag structure, not Dict
        expected = Bag (Bagdict (B1 (Pair (L1 (Ri 30)) (L1 (Ri 40)))) (D1 (L1 (Ri 10)) (L1 (Ri 20)))) (Pair (L1 (Ri 50)) (L1 (Ri 60))) Nil
      result `shouldEqual` (Just expected)
