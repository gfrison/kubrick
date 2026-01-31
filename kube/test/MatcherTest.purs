module Test.Kube.MatcherTest where

import Prelude hiding (add)

import Control.Monad.State (runState)
import Data.Array as Array
import Data.List (List(..), (:))
import Data.List.Lazy as LazyList
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Kubrick.Kube (Kid(..), Kube, addM, addFrom, (+>), emptyKube)
import Kubrick.Matcher (match)
import Kubrick.Types (Raw(..))
import Kubrick.Lem (Lem(..), Bag1(..), Dict1(..), (<+>), (\/), (<+), (+:), lem)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- Helper to add a Lem and return its Kid
addLem :: Lem Raw -> Tuple Kid (Tuple Kid Kube)
addLem l = runState (addM l) (Tuple (Kid 0) emptyKube)

-- Helper to build Kube with multiple Lems and get their Kids
addMultiple :: Array (Lem Raw) -> { kids :: Array Kid, kube :: Tuple Kid Kube }
addMultiple lems = 
  let result = Array.foldl 
        (\acc l -> 
          let newKube /\ newNextKid = (acc.kube /\ acc.nextKid) +> l
          in { kids: acc.kids <> [acc.nextKid], kube: newKube, nextKid: newNextKid }
        )
        { kids: [], kube: emptyKube, nextKid: Kid 0 }
        lems
  in { kids: result.kids, kube: Tuple result.nextKid result.kube }

spec :: Spec Unit
spec = do
  describe "Matcher - L1 matching" do
    
    it "L1 matches exact L1" do
      let
        Tuple kid (Tuple _ kube) = addLem (L1 (Rs "a"))
        result = match kube (L1 (Rs "a"))
      LazyList.length result `shouldEqual` 1
      Set.member kid (Set.fromFoldable result) `shouldEqual` true

    it "L1 matches Sek head (position 0)" do
      let
        Tuple kid (Tuple _ kube) = addLem ((Rs "a") +:   ((Rs "b") +:   ((Rs "c") +:   L0)))
        result = match kube (L1 (Rs "a"))
      LazyList.length result `shouldEqual` 1
      Set.member kid (Set.fromFoldable result) `shouldEqual` true

    it "L1 does NOT match Sek at position > 0" do
      let
        -- Create flat Sek with Sek constructor to avoid nested structure
        Tuple _ (Tuple _ kube) = addLem (Sek (L1 (Rs "a")) (L1 (Rs "b")) ((L1 (Rs "c")) : Nil))
        result = match kube (L1 (Rs "b"))
      LazyList.length result `shouldEqual` 0

    it "L1 matches Bag containing it" do
      let
        -- Create flat Bag: L1 <+> Bag works, but Bag <+> L1 creates nested structure
        Tuple kid (Tuple _ kube) = addLem (L1 (Rs "c") <+> (L1 (Rs "a") <+> L1 (Rs "b")))
        result = match kube (L1 (Rs "b"))
      LazyList.length result `shouldEqual` 1
      Set.member kid (Set.fromFoldable result) `shouldEqual` true

    it "L1 matches Choice containing it" do
      let
        Tuple kid (Tuple _ kube) = addLem (L1 (Rs "a") \/ L1 (Rs "b") \/ L1 (Rs "c"))
        result = match kube (L1 (Rs "b"))
      LazyList.length result `shouldEqual` 1
      Set.member kid (Set.fromFoldable result) `shouldEqual` true

    it "L1 does NOT match in Pair" do
      let
        Tuple _ (Tuple _ kube) = addLem (lem (((Rs "a") /\ (Rs "b")) : Nil))
        result = match kube (L1 (Rs "a"))
      LazyList.length result `shouldEqual` 0

    it "L1 returns empty for no match" do
      let
        Tuple _ (Tuple _ kube) = addLem (L1 (Rs "a"))
        result = match kube (L1 (Rs "x"))
      LazyList.length result `shouldEqual` 0

  describe "Matcher - Sek matching" do
    
    it "Sek matches exact Sek" do
      let
        query :: Lem Raw
        query = (Rs "a") +: ((Rs "b") +: L0)
        Tuple kid (Tuple _ kube) = addLem query
        result = match kube query
      LazyList.length result `shouldEqual` 1
      Set.member kid (Set.fromFoldable result) `shouldEqual` true

    it "Sek matches longer Sek (prefix)" do
      let
        query :: Lem Raw
        query = (Rs "a") +:   ((Rs "b") +:   L0)
        Tuple kid (Tuple _ kube) = addLem (Sek (L1 (Rs "a")) (L1 (Rs "b")) ((L1 (Rs "c")) : Nil))
        result = match kube query
      LazyList.length result `shouldEqual` 1
      Set.member kid (Set.fromFoldable result) `shouldEqual` true

    it "Sek does NOT match if not prefix" do
      let
        query :: Lem Raw
        query = (Rs "b") +:   ((Rs "c") +:   L0)
        Tuple _ (Tuple _ kube) = addLem (Sek (L1 (Rs "a")) (L1 (Rs "b")) ((L1 (Rs "c")) : Nil))
        result = match kube query
      LazyList.length result `shouldEqual` 0

    it "Sek does NOT match shorter Sek" do
      let
        query :: Lem Raw
        query = (Rs "a") +:   ((Rs "b") +:   ((Rs "c") +:   L0))
        Tuple _ (Tuple _ kube) = addLem ((Rs "a") +:   ((Rs "b") +:   L0))
        result = match kube query
      LazyList.length result `shouldEqual` 0

    it "Sek matches Sekdict with matching prefix" do
      let
        sek :: Lem Raw
        sek = (Rs "a") +:   ((Rs "b") +:   L0)
        pair = lem (((Rs "k") /\ (Rs "v")) : Nil)
        Tuple kid (Tuple _ kube) = addLem (sek +: pair)
        result = match kube sek
      LazyList.length result `shouldEqual` 1
      Set.member kid (Set.fromFoldable result) `shouldEqual` true

  describe "Matcher - Bag matching" do
    
    it "Bag matches exact Bag" do
      let
        query :: Lem Raw
        query = L1 (Rs "a") <+> L1 (Rs "b")
        Tuple kid (Tuple _ kube) = addLem query
        result = match kube query
      LazyList.length result `shouldEqual` 1
      Set.member kid (Set.fromFoldable result) `shouldEqual` true

    it "Bag matches superset Bag" do
      let
        query :: Lem Raw
        query = L1 (Rs "a") <+> L1 (Rs "b")
        -- Create flat Bag: L1 <+> Bag works
        Tuple kid (Tuple _ kube) = addLem (L1 (Rs "c") <+> (L1 (Rs "a") <+> L1 (Rs "b")))
        result = match kube query
      LazyList.length result `shouldEqual` 1
      Set.member kid (Set.fromFoldable result) `shouldEqual` true

    it "Bag does NOT match if missing elements" do
      let
        query :: Lem Raw
        query = L1 (Rs "a") <+> L1 (Rs "b") <+> L1 (Rs "c")
        Tuple _ (Tuple _ kube) = addLem (L1 (Rs "a") <+> L1 (Rs "b"))
        result = match kube query
      LazyList.length result `shouldEqual` 0

    it "Bag matches Choice (subset)" do
      let
        query :: Lem Raw
        query = L1 (Rs "a") <+> L1 (Rs "b")
        Tuple kid (Tuple _ kube) = addLem (L1 (Rs "a") \/ L1 (Rs "b") \/ L1 (Rs "c"))
        result = match kube query
      LazyList.length result `shouldEqual` 1
      Set.member kid (Set.fromFoldable result) `shouldEqual` true

  describe "Matcher - Choice matching (OR logic)" do
    
    it "Choice matches if ANY element matches" do
      let
        { kids, kube: Tuple _ kubeData } = addMultiple [L1 (Rs "a"), L1 (Rs "b"), L1 (Rs "c")]
        query :: Lem Raw
        query = L1 (Rs "a") \/ L1 (Rs "x") \/ L1 (Rs "y")
        result = match kubeData query
        getKid arr idx = case Array.index arr idx of
          Just x -> x
          Nothing -> Kid (-1)
      -- Should match the Kid with "a"
      LazyList.length result `shouldEqual` 1
      Set.member (getKid kids 0) (Set.fromFoldable result) `shouldEqual` true

    it "Choice returns multiple Kids if multiple match" do
      let
        { kids, kube: Tuple _ kubeData } = addMultiple [L1 (Rs "a"), L1 (Rs "b"), L1 (Rs "c")]
        query :: Lem Raw
        query = L1 (Rs "a") \/ L1 (Rs "b")
        result = match kubeData query
        getKid arr idx = case Array.index arr idx of
          Just x -> x
          Nothing -> Kid (-1)
      LazyList.length result `shouldEqual` 2
      Set.member (getKid kids 0) (Set.fromFoldable result) `shouldEqual` true
      Set.member (getKid kids 1) (Set.fromFoldable result) `shouldEqual` true

    it "Choice with Sek alternatives" do
      let
        sek1 :: Lem Raw
        sek1 = (Rs "a") +:   ((Rs "b") +:   L0)
        sek2 :: Lem Raw
        sek2 = (Rs "c") +:   ((Rs "d") +:   L0)
        { kids, kube: Tuple _ kubeData } = addMultiple [sek1, sek2]
        query :: Lem Raw
        query = sek1 \/ sek2
        result = match kubeData query
        getKid arr idx = case Array.index arr idx of
          Just x -> x
          Nothing -> Kid (-1)
      LazyList.length result `shouldEqual` 2
      Set.member (getKid kids 0) (Set.fromFoldable result) `shouldEqual` true
      Set.member (getKid kids 1) (Set.fromFoldable result) `shouldEqual` true

  describe "Matcher - Pair matching" do
    
    it "Pair matches exact Pair" do
      let
        query :: Lem Raw
        query = lem (((Rs "k") /\ (Rs "v")) : Nil)
        Tuple kid (Tuple _ kube) = addLem query
        result = match kube query
      LazyList.length result `shouldEqual` 1
      Set.member kid (Set.fromFoldable result) `shouldEqual` true

    it "Pair matches Dict containing it" do
      let
        query :: Lem Raw
        query = lem (((Rs "k1") /\ (Rs "v1")) : Nil)
        Tuple kid (Tuple _ kube) = addLem (lem (((Rs "k1") /\ (Rs "v1")) : Nil) <+ ((Rs "k2") /\ (Rs "v2")))
        result = match kube query
      LazyList.length result `shouldEqual` 1
      Set.member kid (Set.fromFoldable result) `shouldEqual` true

    it "Pair matches Sekdict containing it in Dict part" do
      let
        sek :: Lem Raw
        sek = (Rs "a") +:   ((Rs "b") +:   L0)
        dict = lem (((Rs "k") /\ (Rs "v")) : Nil) <+ ((Rs "k2") /\ (Rs "v2"))
        query :: Lem Raw
        query = lem (((Rs "k") /\ (Rs "v")) : Nil)
        Tuple kid (Tuple _ kube) = addLem (sek +: dict)
        result = match kube query
      LazyList.length result `shouldEqual` 1
      Set.member kid (Set.fromFoldable result) `shouldEqual` true

    it "Pair matches Bagdict containing it in Dict part" do
      let
        bag :: Lem Raw
        bag = L1 (Rs "a") <+> L1 (Rs "b")
        dict = lem (((Rs "k") /\ (Rs "v")) : Nil)
        query :: Lem Raw
        query = lem (((Rs "k") /\ (Rs "v")) : Nil)
        Tuple kid (Tuple _ kube) = addLem (bag <+> dict)
        result = match kube query
      LazyList.length result `shouldEqual` 1
      Set.member kid (Set.fromFoldable result) `shouldEqual` true

    it "Pair does NOT match if key differs" do
      let
        query :: Lem Raw
        query = lem (((Rs "x") /\ (Rs "v")) : Nil)
        Tuple _ (Tuple _ kube) = addLem (lem (((Rs "k") /\ (Rs "v")) : Nil))
        result = match kube query
      LazyList.length result `shouldEqual` 0

    it "Pair does NOT match if value differs" do
      let
        query :: Lem Raw
        query = lem (((Rs "k") /\ (Rs "x")) : Nil)
        Tuple _ (Tuple _ kube) = addLem (lem (((Rs "k") /\ (Rs "v")) : Nil))
        result = match kube query
      LazyList.length result `shouldEqual` 0

  describe "Matcher - Dict matching" do
    
    it "Dict matches exact Dict" do
      let
        query :: Lem Raw
        query = lem (((Rs "k1") /\ (Rs "v1")) : Nil) <+ ((Rs "k2") /\ (Rs "v2"))
        Tuple kid (Tuple _ kube) = addLem query
        result = match kube query
      LazyList.length result `shouldEqual` 1
      Set.member kid (Set.fromFoldable result) `shouldEqual` true

    it "Dict matches superset Dict" do
      let
        query :: Lem Raw
        query = lem (((Rs "k1") /\ (Rs "v1")) : Nil) <+ ((Rs "k2") /\ (Rs "v2"))
        Tuple kid (Tuple _ kube) = addLem (lem (((Rs "k1") /\ (Rs "v1")) : Nil) <+ ((Rs "k2") /\ (Rs "v2")) <+ ((Rs "k3") /\ (Rs "v3")))
        result = match kube query
      LazyList.length result `shouldEqual` 1
      Set.member kid (Set.fromFoldable result) `shouldEqual` true

    it "Dict does NOT match if missing pairs" do
      let
        query :: Lem Raw
        query = lem (((Rs "k1") /\ (Rs "v1")) : Nil) <+ ((Rs "k2") /\ (Rs "v2"))
        Tuple _ (Tuple _ kube) = addLem (lem (((Rs "k1") /\ (Rs "v1")) : Nil))
        result = match kube query
      LazyList.length result `shouldEqual` 0

    it "Dict matches Sekdict containing all pairs" do
      let
        sek :: Lem Raw
        sek = (Rs "a") +:   ((Rs "b") +:   L0)
        dict = lem (((Rs "k1") /\ (Rs "v1")) : Nil) <+ ((Rs "k2") /\ (Rs "v2"))
        query :: Lem Raw
        query = lem (((Rs "k1") /\ (Rs "v1")) : Nil) <+ ((Rs "k2") /\ (Rs "v2"))
        Tuple kid (Tuple _ kube) = addLem (sek +: dict)
        result = match kube query
      LazyList.length result `shouldEqual` 1
      Set.member kid (Set.fromFoldable result) `shouldEqual` true

    it "Dict matches Bagdict containing all pairs" do
      let
        -- Create Bagdict directly: Bagdict (Bag part) (Dict part)
        bag :: Bag1 Raw
        bag = B2 (L1 (Rs "a")) (L1 (Rs "b")) Nil
        dict :: Dict1 Raw
        dict = D2 (Tuple (L1 (Rs "k1")) (L1 (Rs "v1"))) (Tuple (L1 (Rs "k2")) (L1 (Rs "v2"))) Nil
        bagdict = Bagdict bag dict
        query :: Lem Raw
        query = lem (((Rs "k1") /\ (Rs "v1")) : ((Rs "k2") /\ (Rs "v2")) : Nil)
        Tuple kid (Tuple _ kube) = addLem bagdict
        result = match kube query
      LazyList.length result `shouldEqual` 1
      Set.member kid (Set.fromFoldable result) `shouldEqual` true

  describe "Matcher - Multiple matches" do
    
    it "Query matches multiple different Kids" do
      let
        sek1 :: Lem Raw
        sek1 = (Rs "a") +:   ((Rs "b") +:   L0)
        sek2 :: Lem Raw
        sek2 = Sek (L1 (Rs "a")) (L1 (Rs "b")) ((L1 (Rs "c")) : Nil)
        sek3 :: Lem Raw
        sek3 = Sek (L1 (Rs "a")) (L1 (Rs "b")) ((L1 (Rs "d")) : Nil)
        { kids, kube: Tuple _ kubeData } = addMultiple [sek1, sek2, sek3]
        query :: Lem Raw
        query = (Rs "a") +:   ((Rs "b") +:   L0)
        result = match kubeData query
        getKid arr idx = case Array.index arr idx of
          Just x -> x
          Nothing -> Kid (-1)
      -- All three Seks have prefix "a" "b"
      LazyList.length result `shouldEqual` 3
      Set.member (getKid kids 0) (Set.fromFoldable result) `shouldEqual` true
      Set.member (getKid kids 1) (Set.fromFoldable result) `shouldEqual` true
      Set.member (getKid kids 2) (Set.fromFoldable result) `shouldEqual` true

    it "L1 matches multiple structures containing it" do
      let
        sek :: Lem Raw
        sek = (Rs "a") +:   ((Rs "b") +:   L0)
        bag :: Lem Raw
        bag = L1 (Rs "a") <+> L1 (Rs "b")
        choice :: Lem Raw
        choice = L1 (Rs "a") \/ L1 (Rs "b")
        { kids, kube: Tuple _ kubeData } = addMultiple [L1 (Rs "a"), sek, bag, choice]
        result = match kubeData (L1 (Rs "a"))
        getKid arr idx = case Array.index arr idx of
          Just x -> x
          Nothing -> Kid (-1)
      -- L1 (Rs "a"), Sek head, Bag containing, Choice containing
      LazyList.length result `shouldEqual` 4
      Set.member (getKid kids 0) (Set.fromFoldable result) `shouldEqual` true
      Set.member (getKid kids 1) (Set.fromFoldable result) `shouldEqual` true
      Set.member (getKid kids 2) (Set.fromFoldable result) `shouldEqual` true
      Set.member (getKid kids 3) (Set.fromFoldable result) `shouldEqual` true

  describe "Matcher - Empty results" do
    
    it "Returns empty when no L1 match" do
      let
        Tuple _ (Tuple _ kube) = addLem (L1 (Rs "a"))
        result = match kube (L1 (Rs "x"))
      LazyList.length result `shouldEqual` 0

    it "Returns empty when Sek prefix doesn't match" do
      let
        query :: Lem Raw
        query = (Rs "x") +:   ((Rs "y") +:   L0)
        Tuple _ (Tuple _ kube) = addLem ((Rs "a") +:   ((Rs "b") +:   L0))
        result = match kube query
      LazyList.length result `shouldEqual` 0

    it "Returns empty when Bag is not subset" do
      let
        query :: Lem Raw
        query = L1 (Rs "x") <+> L1 (Rs "y")
        Tuple _ (Tuple _ kube) = addLem (L1 (Rs "a") <+> L1 (Rs "b"))
        result = match kube query
      LazyList.length result `shouldEqual` 0

    it "Returns empty when Pair key-value not found" do
      let
        query :: Lem Raw
        query = lem (((Rs "x") /\ (Rs "y")) : Nil)
        Tuple _ (Tuple _ kube) = addLem (lem (((Rs "k") /\ (Rs "v")) : Nil))
        result = match kube query
      LazyList.length result `shouldEqual` 0

    it "Returns empty when querying empty Kube" do
      let
        kube = emptyKube
        result = match kube (L1 (Rs "a"))
      LazyList.length result `shouldEqual` 0

  describe "Matcher - L0 edge cases" do
    
    it "L0 returns empty" do
      let
        Tuple _ (Tuple _ kube) = addLem (L1 (Rs "a"))
        result = match kube L0
      LazyList.length result `shouldEqual` 0

  describe "Matcher - Complex scenarios" do
    
    it "Nested Sek matching" do
      let
        sek1 :: Lem Raw
        sek1 = (Rs "a") +:   ((Rs "b") +:   L0)
        sek2 :: Lem Raw
        sek2 = (Rs "c") +:   ((Rs "d") +:   L0)
        outer :: Lem Raw
        outer = Sek sek1 sek2 Nil
        Tuple kid (Tuple _ kube) = addLem outer
        result = match kube sek1
      -- The outer Sek starts with sek1 at position 0
      LazyList.length result `shouldEqual` 1
      Set.member kid (Set.fromFoldable result) `shouldEqual` true

    it "Bag with nested Lems" do
      let
        sek1 :: Lem Raw
        sek1 = (Rs "a") +:   ((Rs "b") +:   L0)
        sek2 :: Lem Raw
        sek2 = (Rs "c") +:   ((Rs "d") +:   L0)
        bag :: Lem Raw
        bag = Bag sek1 sek2 (Cons (L1 (Rs "e")) Nil)
        query :: Lem Raw
        query = Bag sek1 sek2 Nil
        Tuple kid (Tuple _ kube) = addLem bag
        result = match kube query
      LazyList.length result `shouldEqual` 1
      Set.member kid (Set.fromFoldable result) `shouldEqual` true

    it "Choice with complex alternatives" do
      let
        pair :: Lem Raw
        pair = lem (((Rs "k1") /\ (Rs "v1")) : Nil)
        sek :: Lem Raw
        sek = (Rs "a") +:   ((Rs "b") +:   L0)
        { kids, kube: Tuple _ kubeData } = addMultiple [pair, sek]
        query :: Lem Raw
        query = pair \/ sek
        result = match kubeData query
        getKid arr idx = case Array.index arr idx of
          Just x -> x
          Nothing -> Kid (-1)
      -- Both alternatives should match
      LazyList.length result `shouldEqual` 2
      Set.member (getKid kids 0) (Set.fromFoldable result) `shouldEqual` true
      Set.member (getKid kids 1) (Set.fromFoldable result) `shouldEqual` true
      Set.member (getKid kids 1) (Set.fromFoldable result) `shouldEqual` true
