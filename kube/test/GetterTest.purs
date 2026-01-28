module Test.GetterTest where

import Prelude

import Kubrick.Kube (Kid(..), Kube, bi0)
import Kubrick.Getter as Getter
import Data.List (List(Nil))
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(..))
import Kubrick.Lem (Lem(..), (<+>), (<+), (+:), choice)
import Kubrick.Kube.Types (put)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Getter - Reconstructing Lem from Kube" do

    it "simple sek" do
      -- val k = Kube(seqs = ArraySeq(M2m("a" -> Kid(1)), M2m("b" -> Kid(1))), roots = Set(Kid(1)))
      -- k.get(Kid(1)) should contain(Sek("a", "b"))
      let
        seq0 = put 1 (Kid 1) bi0  -- position 0: "a" -> Kid(1)
        seq1 = put 2 (Kid 1) bi0  -- position 1: "b" -> Kid(1)
        kube :: Kube Int
        kube = { seqs: [seq0, seq1]
               , refSeqs: []
               , keys: bi0
               , refKeys: bi0
               , vals: bi0
               , refVals: bi0
               , roots: Set.singleton (Kid 1)
               , sets: Set.empty
               }
        result = Getter.get ((Kid 1) /\ kube)
        expected = 1 +: (2 +: L0)
      result `shouldEqual` (Just expected)

    it "nested sek" do
      -- val k = Kube(
      --   seqs = ArraySeq(M2m("a" -> Kid(1), "a2" -> Kid(2)), M2m(Kid(2) -> Kid(1), "b2" -> Kid(2))),
      --   roots = Set(Kid(1))
      -- )
      -- k.get(Kid(1)) should contain(Sek(L1("a"), Sek(L1("a2"), L1("b2"))))
      let
        seq0 = put 1 (Kid 1) $ put 2 (Kid 2) bi0  -- "a" -> Kid(1), "a2" -> Kid(2)
        refSeq1 = put (Kid 2) (Kid 1) bi0  -- Kid(2) -> Kid(1)
        seq1 = put 3 (Kid 2) bi0  -- "b2" -> Kid(2)
        kube :: Kube Int
        kube = { seqs: [seq0, seq1]
               , refSeqs: [bi0, refSeq1]
               , keys: bi0
               , refKeys: bi0
               , vals: bi0
               , refVals: bi0
               , roots: Set.singleton (Kid 1)
               , sets: Set.empty
               }
        result = Getter.get ((Kid 1) /\ kube)
        -- The Kube structure creates nested Sek: Kid1 contains [L1 1, Kid2], Kid2 contains [L1 2, L1 3]
        -- Result: Sek with L1 1 and nested Sek (L1 2, L1 3)
        expected = Sek (L1 1) (2 +: (3 +: L0)) Nil
      result `shouldEqual` (Just expected)

    it "set" do
      -- val k = Kube(
      --   keys = M2m("a" -> Kid(1), "b" -> Kid(1)),
      --   roots = Set(Kid(1)),
      --   sets = Set(Kid(1))
      -- )
      -- k.get(Kid(1)) should contain(L0 + "a" + "b")
      let
        keys = put 1 (Kid 1) $ put 2 (Kid 1) bi0  -- "a" -> Kid(1), "b" -> Kid(1)
        kube :: Kube Int
        kube = { seqs: []
               , refSeqs: []
               , keys: keys
               , refKeys: bi0
               , vals: bi0
               , refVals: bi0
               , roots: Set.singleton (Kid 1)
               , sets: Set.singleton (Kid 1)
               }
        result = Getter.get ((Kid 1) /\ kube)
        -- L0 + "a" + "b" creates a Bag
        expected = (L1 1) <+> (L1 2)
      result `shouldEqual` (Just expected)

    it "choice" do
      -- val k = Kube(
      --   keys = M2m("a" -> Kid(1), "b" -> Kid(1)),
      --   roots = Set(Kid(1))
      -- )
      -- k.get(Kid(1)) should contain(L0 || "a" || "b")
      let
        keys = put 1 (Kid 1) $ put 2 (Kid 1) bi0  -- "a" -> Kid(1), "b" -> Kid(1)
        kube :: Kube Int
        kube = { seqs: []
               , refSeqs: []
               , keys: keys
               , refKeys: bi0
               , vals: bi0
               , refVals: bi0
               , roots: Set.singleton (Kid 1)
               , sets: Set.empty
               }
        result = Getter.get ((Kid 1) /\ kube)
        -- Choice of "a" and "b" - Getter reconstructs as Choice from keys without sets flag
        expected = choice 1 2 Nil
      result `shouldEqual` (Just expected)

    it "pair" do
      -- val k = Kube(
      --   keys = M2m("a" -> Kid(2)),
      --   vals = M2m("b" -> Kid(2)),
      --   roots = Set(Kid(2))
      -- )
      -- k.get(Kid(2)) should contain(Pair(L1("a"), L1("b")))
      let
        keys = put 1 (Kid 2) bi0  -- "a" -> Kid(2)
        vals = put 2 (Kid 2) bi0  -- "b" -> Kid(2)
        kube :: Kube Int
        kube = { seqs: []
               , refSeqs: []
               , keys: keys
               , refKeys: bi0
               , vals: vals
               , refVals: bi0
               , roots: Set.singleton (Kid 2)
               , sets: Set.empty
               }
        result = Getter.get ((Kid 2) /\ kube)
        expected = Pair (L1 1) (L1 2)  -- Single Pair is ok
      result `shouldEqual` (Just expected)

    it "sek in choice" do
      -- val k = Kube(
      --   seqs = ArraySeq(M2m("a" -> Kid(1)), M2m("b" -> Kid(1))),
      --   keys = M2m(Kid(1) -> Kid(2), "c" -> Kid(2)),
      --   roots = Set(Kid(2)),
      --   sets = Set(Kid(2))
      -- )
      -- k.get(Kid(2)) should contain(Sek.from(ArraySeq.empty, Set(Sek("a", "b"), L1("c"))))
      let
        seq0 = put 1 (Kid 1) bi0  -- "a" -> Kid(1)
        seq1 = put 2 (Kid 1) bi0  -- "b" -> Kid(1)
        refKeys = put (Kid 1) (Kid 2) bi0  -- Kid(1) -> Kid(2)
        keys = put 3 (Kid 2) bi0  -- "c" -> Kid(2)
        kube :: Kube Int
        kube = { seqs: [seq0, seq1]
               , refSeqs: []
               , keys: keys
               , refKeys: refKeys
               , vals: bi0
               , refVals: bi0
               , roots: Set.singleton (Kid 2)
               , sets: Set.singleton (Kid 2)
               }
        result = Getter.get ((Kid 2) /\ kube)
        -- Set containing Sek("a", "b") and L1("c")
        sek = 1 +: (2 +: L0)
        expected = sek <+> (L1 3)
      result `shouldEqual` (Just expected)

    it "set and sek" do
      -- val k = Kube(
      --   seqs = ArraySeq(M2m("a" -> Kid(1)), M2m("b" -> Kid(1))),
      --   keys = M2m("c" -> Kid(1)),
      --   roots = Set(Kid(1))
      -- )
      -- k.get(Kid(1)) should contain(Sek("a", "b") + "c")
      let
        seq0 = put 1 (Kid 1) bi0  -- "a" -> Kid(1)
        seq1 = put 2 (Kid 1) bi0  -- "b" -> Kid(1)
        keys = put 3 (Kid 1) bi0  -- "c" -> Kid(1)
        kube :: Kube Int
        kube = { seqs: [seq0, seq1]
               , refSeqs: []
               , keys: keys
               , refKeys: bi0
               , vals: bi0
               , refVals: bi0
               , roots: Set.singleton (Kid 1)
               , sets: Set.empty
               }
        result = Getter.get ((Kid 1) /\ kube)
        sek = 1 +: (2 +: L0)
        expected = sek <+ 3
      result `shouldEqual` (Just expected)

    it "sek + choice" do
      -- val k = Kube(
      --   seqs = ArraySeq(M2m("a" -> Kid(1)), M2m("b" -> Kid(1))),
      --   keys = M2m("c" -> Kid(1), "d" -> Kid(1)),
      --   roots = Set(Kid(1))
      -- )
      -- val sek = Sek("a", "b") + Choice("c", "d")
      -- k.get(Kid(1)) should contain(sek)
      let
        seq0 = put 1 (Kid 1) bi0  -- "a" -> Kid(1)
        seq1 = put 2 (Kid 1) bi0  -- "b" -> Kid(1)
        keys = put 3 (Kid 1) $ put 4 (Kid 1) bi0  -- "c" -> Kid(1), "d" -> Kid(1)
        kube :: Kube Int
        kube = { seqs: [seq0, seq1]
               , refSeqs: []
               , keys: keys
               , refKeys: bi0
               , vals: bi0
               , refVals: bi0
               , roots: Set.singleton (Kid 1)
               , sets: Set.empty
               }
        result = Getter.get ((Kid 1) /\ kube)
        sek = 1 +: (2 +: L0)
        choiceElem = choice 3 4 Nil
        expected = sek <+> choiceElem
      result `shouldEqual` (Just expected)