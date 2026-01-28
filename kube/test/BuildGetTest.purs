module Test.BuildGetTest where

import Prelude hiding (add)

import Kubrick.Kube (Kid(..), Kube, emptyKube, add)
import Kubrick.Getter as Getter
import Control.Monad.State (runState)
import Data.List ((:), List(Nil))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Kubrick.Lem (Lem(..), (+:), choice, dict, Bag1(..), Dict1(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "BuildGet - Round-trip tests (build with add, retrieve with get)" do

    it "reconstructs structure built with add" do
      -- Integration test: build with add, then get back
      let
        original = 1 +: (2 +: (3 +: L0))
        Tuple kid (Tuple _ kube) = runState (add original) (Tuple (Kid 0) (emptyKube :: Kube Int))
        result = Getter.get (kid /\ kube)
      result `shouldEqual` (Just original)

    it "reconstructs Pair built with add" do
      let
        original = Pair (L1 1) (L1 2)  -- Single Pair is ok
        Tuple kid (Tuple _ kube) = runState (add original) (Tuple (Kid 0) (emptyKube :: Kube Int))
        result = Getter.get (kid /\ kube)
      result `shouldEqual` (Just original)

    it "reconstructs Choice built with add" do
      let
        original = choice 1 2 (3 : Nil)
        Tuple kid (Tuple _ kube) = runState (add original) (Tuple (Kid 0) (emptyKube :: Kube Int))
        result = Getter.get (kid /\ kube)
      result `shouldEqual` (Just original)

    it "reconstructs simple Dict (Pair form)" do
      let
        -- Single key-value pair becomes Pair
        original = Pair (L1 100) (L1 200)  -- Single Pair is ok
        Tuple kid (Tuple _ kube) = runState (add original) (Tuple (Kid 0) (emptyKube :: Kube Int))
        result = Getter.get (kid /\ kube)
      result `shouldEqual` (Just original)

    it "reconstructs Sek with L1 elements" do
      let
        original = 100 +: (200 +: (300 +: (400 +: L0)))
        Tuple kid (Tuple _ kube) = runState (add original) (Tuple (Kid 0) (emptyKube :: Kube Int))
        result = Getter.get (kid /\ kube)
      result `shouldEqual` (Just original)

    it "reconstructs Dict as Bagdict (Dict doesn't preserve structure)" do
      let
        -- Dict creates a dictionary, but round-trip changes it to Bagdict
        original = dict (1000 /\ 1100) (2000 /\ 2200) Nil
        Tuple kid (Tuple _ kube) = runState (add original) (Tuple (Kid 0) (emptyKube :: Kube Int))
        result = Getter.get (kid /\ kube)
        -- Expect Bagdict, not Dict - this is what Getter reconstructs
        expected = Bagdict (B1 (Pair (L1 2000) (L1 2200))) (D1 (L1 1000) (L1 1100))
      result `shouldEqual` (Just expected)

    it "reconstructs Choice with simple elements" do
      let
        original = choice 100 200 (300 : 400 : Nil)
        Tuple kid (Tuple _ kube) = runState (add original) (Tuple (Kid 0) (emptyKube :: Kube Int))
        result = Getter.get (kid /\ kube)
      result `shouldEqual` (Just original)

    it "round-trip preserves L1 values" do
      let
        original = L1 12345
        Tuple kid (Tuple _ kube) = runState (add original) (Tuple (Kid 0) (emptyKube :: Kube Int))
        result = Getter.get (kid /\ kube)
      result `shouldEqual` (Just original)

    it "round-trip preserves simple Pair" do
      let
        original = Pair (L1 5000) (L1 6000)  -- Single Pair is ok
        Tuple kid (Tuple _ kube) = runState (add original) (Tuple (Kid 0) (emptyKube :: Kube Int))
        result = Getter.get (kid /\ kube)
      result `shouldEqual` (Just original)

    it "round-trip preserves simple Sek" do
      let
        original = 9000 +: (9001 +: (9002 +: L0))
        Tuple kid (Tuple _ kube) = runState (add original) (Tuple (Kid 0) (emptyKube :: Kube Int))
        result = Getter.get (kid /\ kube)
      result `shouldEqual` (Just original)

    it "round-trip preserves simple Choice" do
      let
        original = choice 7000 7001 (7002 : Nil)
        Tuple kid (Tuple _ kube) = runState (add original) (Tuple (Kid 0) (emptyKube :: Kube Int))
        result = Getter.get (kid /\ kube)
      result `shouldEqual` (Just original)

    it "Dict with 3+ pairs becomes Bag structure on round-trip" do
      let
        -- Dict with 3+ pairs doesn't preserve as Dict
        original = dict (10 /\ 20) (30 /\ 40) ((50 /\ 60) : Nil)
        Tuple kid (Tuple _ kube) = runState (add original) (Tuple (Kid 0) (emptyKube :: Kube Int))
        result = Getter.get (kid /\ kube)
        -- Expect Bag structure, not Dict
        expected = Bag (Bagdict (B1 (Pair (L1 30) (L1 40))) (D1 (L1 10) (L1 20))) (Pair (L1 50) (L1 60)) Nil
      result `shouldEqual` (Just expected)
