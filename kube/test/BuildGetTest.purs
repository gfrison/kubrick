module Test.BuildGetTest where

import Prelude hiding (add)

import Kubrick.Kube (Kid(..), emptyKube, addM)
import Kubrick.Getter as Getter
import Control.Monad.State (runState)
import Data.List ((:), List(Nil))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Kubrick.Types (Raw(..))
import Kubrick.Lem (Lem(..), (+:), (<+), (\/), lem)
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

    it "reconstructs Dict correctly (Dict now preserves structure)" do
      let
        -- Dict creates a dictionary and round-trip preserves it as Dict
        original = ((Ri 2000) /\ (Ri 2200)) <+ lem (((Ri 1000) /\ (Ri 1100)) : Nil)
        Tuple kid (Tuple _ kube) = runState (addM original) (Tuple (Kid 0) (emptyKube))
        result = Getter.get kube kid
        -- Expect Dict to be preserved
        expected = Dict (Tuple (L1 (Ri 1000)) (L1 (Ri 1100))) (Tuple (L1 (Ri 2000)) (L1 (Ri 2200))) Nil
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

    it "Dict with 3+ pairs preserves Dict structure on round-trip" do
      let
        -- Dict with 3+ pairs now preserves as Dict
        original = ((Ri 50) /\ (Ri 60)) <+ (((Ri 30) /\ (Ri 40)) <+ lem (((Ri 10) /\ (Ri 20)) : Nil))
        Tuple kid (Tuple _ kube) = runState (addM original) (Tuple (Kid 0) (emptyKube))
        result = Getter.get kube kid
        -- Expect Dict to be preserved
        expected = Dict (Tuple (L1 (Ri 10)) (L1 (Ri 20))) (Tuple (L1 (Ri 30)) (L1 (Ri 40))) ((Tuple (L1 (Ri 50)) (L1 (Ri 60))) : Nil)
      result `shouldEqual` (Just expected)
