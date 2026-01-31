module Test.Kube.BuilderTest where

import Prelude hiding (add)

import Control.Monad.State (runState)
import Data.Array as Array
import Data.List (List(..), (:))
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Kubrick.Kube (Kid(..), add, (+), addM, emptyKube)
import Kubrick.Kube.Types (getKeys, getValues)
import Kubrick.Kube.Types as Kubrick.Kube.Types
import Kubrick.Types (Raw(..))
import Kubrick.Lem (Lem(..), (<+>), (\/), (<+), (+:), (:::), lem)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- Helper to get Bi at position in seqs array
getBiAt :: forall a. Ord a => Int -> Array (Kubrick.Kube.Types.M2m a Kid) -> Kubrick.Kube.Types.M2m a Kid
getBiAt idx arr = fromMaybe (Kubrick.Kube.Types.M2m Map.empty Map.empty) (Array.index arr idx)

-- Helper to check if a mapping exists in a Bi
hasMappingValue :: forall k v. Ord k => Ord v => k -> v -> Kubrick.Kube.Types.M2m k v -> Boolean
hasMappingValue k v bi = Set.member v (getValues bi k)

hasMappingKey :: forall k v. Ord k => Ord v => v -> k -> Kubrick.Kube.Types.M2m k v -> Boolean
hasMappingKey v k bi = Set.member k (getKeys bi v)

spec :: Spec Unit
spec = do
  describe "Builder - Examples from Comments" do

    describe "Example 1: L1 a" do
      it "L1 a -> {seqs = [{a -> k0}], roots = {k0}}" do
        let
          Tuple kid (Tuple _ kube) = runState (addM (L1 (Rs "a"))) (Tuple (Kid 0) emptyKube)
        -- roots should contain exactly one Kid
        Set.size kube.roots `shouldEqual` 1
        -- kid should be in roots
        Set.member kid kube.roots `shouldEqual` true
        -- seqs should have 1 position
        Array.length kube.seqs `shouldEqual` 1
        let bi0 = getBiAt 0 kube.seqs
        -- "a" should map to the same Kid as returned
        hasMappingValue (Rs "a") kid bi0 `shouldEqual` true

    describe "Example 2: Sek a b" do
      it "Sek a b -> {seqs = [{a -> k0}, {b -> k0}], roots = {k0}}" do
        let
          input = (Rs "a") +:   ((Rs "b") +:   L0)
          (kube /\ _) = emptyKube + input
          kid = Kid 0  -- The Kid assigned to the inserted Lem
        -- roots should contain exactly one Kid
        Set.size kube.roots `shouldEqual` 1
        -- kid should be in roots
        Set.member kid kube.roots `shouldEqual` true
        -- seqs should have 2 positions
        Array.length kube.seqs `shouldEqual` 2
        let bi0 = getBiAt 0 kube.seqs
        let bi1 = getBiAt 1 kube.seqs
        -- Both "a" and "b" should map to the same Kid
        hasMappingValue (Rs "a") kid bi0 `shouldEqual` true
        hasMappingValue (Rs "b") kid bi1 `shouldEqual` true

    describe "Example 3: Bag a b" do
      it "Bag a b -> {keys = {a -> k0, b -> k0}, roots = {k0}, sets = {k0}}" do
        let
          input = L1 (Rs "a") <+> L1 (Rs "b")
          (kube /\ _) = emptyKube + input
          kid = Kid 0
        -- roots and sets should contain exactly one Kid
        Set.size kube.roots `shouldEqual` 1
        Set.size kube.sets `shouldEqual` 1
        -- kid should be in both roots and sets
        Set.member kid kube.roots `shouldEqual` true
        Set.member kid kube.sets `shouldEqual` true
        -- Both "a" and "b" should map to the same Kid
        hasMappingValue (Rs "a") kid kube.keys `shouldEqual` true
        hasMappingValue (Rs "b") kid kube.keys `shouldEqual` true

    describe "Example 4: Choice a b" do
      it "Choice a b -> {keys = {a -> k0, b -> k0}, roots = {k0}}" do
        let
          input = L1 (Rs "a") \/ L1 (Rs "b")
          (kube /\ _) = emptyKube + input
          kid = Kid 0
        -- roots should contain exactly one Kid
        Set.size kube.roots `shouldEqual` 1
        -- kid should be in roots
        Set.member kid kube.roots `shouldEqual` true
        -- Both "a" and "b" should map to the same Kid
        hasMappingValue (Rs "a") kid kube.keys `shouldEqual` true
        hasMappingValue (Rs "b") kid kube.keys `shouldEqual` true
        -- sets should be empty (not a Bag)
        Set.isEmpty kube.sets `shouldEqual` true

    describe "Example 5: Pair a b" do
      it "Pair a b -> {keys = {a -> k0}, vals = {b -> k0}, roots = {k0}}" do
        let
          input = lem (((Rs "a") /\ (Rs "b")) : Nil)
          (kube /\ _) = emptyKube + input
          kid = Kid 0
        -- roots should contain exactly one Kid
        Set.size kube.roots `shouldEqual` 1
        -- kid should be in roots
        Set.member kid kube.roots `shouldEqual` true
        -- "a" and "b" should map to the same Kid (the pair)
        hasMappingValue (Rs "a") kid kube.keys `shouldEqual` true
        hasMappingValue (Rs "b") kid kube.vals `shouldEqual` true

    describe "Example 6: Dict (ka /\\ va) (kb /\\ vb)" do
      it "Dict (ka /\\ va) (kb /\\ vb) -> {keys = {ka -> k0, kb -> k1}, vals = {va -> k0, vb -> k1}, refKeys = {k2 -> {k0, k1}}, roots = {k2}}" do
        let
          input = lem (((Rs "ka") /\ (Rs "va")) : Nil) <+ ((Rs "kb") /\ (Rs "vb"))
          (kube /\ _) = emptyKube + input
          dictKid = Kid 2
        -- roots should contain exactly one Kid (the Dict)
        Set.size kube.roots `shouldEqual` 1
        Set.member dictKid kube.roots `shouldEqual` true
        -- Find the pair Kids
        let pairKid1Set = getValues kube.keys (Rs "ka")
        let pairKid2Set = getValues kube.keys (Rs "kb")
        Set.size pairKid1Set `shouldEqual` 1
        Set.size pairKid2Set `shouldEqual` 1
        let pairKid1 = fromMaybe (Kid (-1)) (Set.findMin pairKid1Set)
        let pairKid2 = fromMaybe (Kid (-1)) (Set.findMin pairKid2Set)
        -- The two pairs should be different Kids
        (pairKid1 /= pairKid2) `shouldEqual` true
        -- ka and va should map to the same pair Kid
        hasMappingValue (Rs "ka") pairKid1 kube.keys `shouldEqual` true
        hasMappingValue (Rs "va") pairKid1 kube.vals `shouldEqual` true
        -- kb and vb should map to the same pair Kid
        hasMappingValue (Rs "kb") pairKid2 kube.keys `shouldEqual` true
        hasMappingValue (Rs "vb") pairKid2 kube.vals `shouldEqual` true
        -- Both pairs should reference the dict Kid
        hasMappingValue pairKid1 dictKid kube.refKeys `shouldEqual` true
        hasMappingValue pairKid2 dictKid kube.refKeys `shouldEqual` true

    describe "Example 7: (a +: b +: L0) + (c /\\ d)" do
      it "(a +: b +: L0) + (c /\\ d) -> {seqs = [{a -> k0}, {b -> k0}], keys = {c -> k1}, vals = {d -> k1}, refKeys = {k0 -> {k1}}, roots = {k0}}" do
        let
          sek :: Lem Raw
          sek = (Rs "a") +:   ((Rs "b") +:   L0)
          pair = lem (((Rs "c") /\ (Rs "d")) : Nil)
          input = sek +:  pair  -- Prepend Sek to Pair creates Sekdict
          Tuple sekdictKid (Tuple _ kube) = runState (addM input) (Tuple (Kid 0) emptyKube)
        -- Check if it's actually a Sekdict
        case input of
          Sekdict _ _ -> true `shouldEqual` true
          _ -> false `shouldEqual` true
        -- roots should contain exactly one Kid (the Sekdict)
        Set.size kube.roots `shouldEqual` 1
        Set.member sekdictKid kube.roots `shouldEqual` true
        -- seqs should have 2 positions
        Array.length kube.seqs `shouldEqual` 2
        let bi0 = getBiAt 0 kube.seqs
        let bi1 = getBiAt 1 kube.seqs
        -- "a" and "b" should both map to the Sekdict Kid
        hasMappingValue (Rs "a") sekdictKid bi0 `shouldEqual` true
        hasMappingValue (Rs "b") sekdictKid bi1 `shouldEqual` true
        -- Find the pair Kid via "c"
        let pairKidSet = getValues kube.keys (Rs "c")
        Set.size pairKidSet `shouldEqual` 1
        let pairKid = fromMaybe (Kid (-1)) (Set.findMin pairKidSet)
        -- "c" and "d" should map to the same pair Kid
        hasMappingValue (Rs "c") pairKid kube.keys `shouldEqual` true
        hasMappingValue (Rs "d") pairKid kube.vals `shouldEqual` true
        -- The pair should reference the Sekdict
        hasMappingValue pairKid sekdictKid kube.refKeys `shouldEqual` true

    describe "Example 8: Sek(Sek(a b) Sek(c d))" do
      it "Sek(Sek(a b) Sek(c d)) -> {seqs = [{a -> k0, c -> k1}, {b -> k0, d -> k1}], refSeqs = [{k0 -> k2}, {k1 -> k2}], roots = {k2}}" do
        let
          sek1 :: Lem Raw
          sek1 = (Rs "a") +:   ((Rs "b") +:   L0)
          sek2 :: Lem Raw
          sek2 = (Rs "c") +:   ((Rs "d") +:   L0)
          input = sek1 ::: sek2
          (kube /\ _) = emptyKube + input
          outerKid = Kid 2
        -- roots should contain exactly one Kid (the outer Sek)
        Set.size kube.roots `shouldEqual` 1
        Set.member outerKid kube.roots `shouldEqual` true
        -- seqs should have 2 positions
        Array.length kube.seqs `shouldEqual` 2
        let bi0 = getBiAt 0 kube.seqs
        let bi1 = getBiAt 1 kube.seqs
        -- Find the inner Sek Kids
        let innerKid1Set = getValues bi0 (Rs "a")
        let innerKid2Set = getValues bi0 (Rs "c")
        Set.size innerKid1Set `shouldEqual` 1
        Set.size innerKid2Set `shouldEqual` 1
        let innerKid1 = fromMaybe (Kid (-1)) (Set.findMin innerKid1Set)
        let innerKid2 = fromMaybe (Kid (-1)) (Set.findMin innerKid2Set)
        -- The two inner Seks should be different Kids
        (innerKid1 /= innerKid2) `shouldEqual` true
        -- "a" and "b" should map to the same inner Sek Kid
        hasMappingValue (Rs "a") innerKid1 bi0 `shouldEqual` true
        hasMappingValue (Rs "b") innerKid1 bi1 `shouldEqual` true
        -- "c" and "d" should map to the same inner Sek Kid
        hasMappingValue (Rs "c") innerKid2 bi0 `shouldEqual` true
        hasMappingValue (Rs "d") innerKid2 bi1 `shouldEqual` true
        -- Both inner Seks should reference the outer Sek
        (Array.length kube.refSeqs >= 2) `shouldEqual` true
        let refBi0 = getBiAt 0 kube.refSeqs
        let refBi1 = getBiAt 1 kube.refSeqs
        hasMappingValue innerKid1 outerKid refBi0 `shouldEqual` true
        hasMappingValue innerKid2 outerKid refBi1 `shouldEqual` true

    describe "Example 9: Sekdict (Sek(a b) Sek(c d)) Dict (e -> f) (g -> i)" do
      it "Sekdict complex -> {seqs, refSeqs, keys, vals, refKeys, roots}" do
        let
          sek1 :: Lem Raw
          sek1 = (Rs "a") +:   ((Rs "b") +:   L0)
          sek2 :: Lem Raw
          sek2 = (Rs "c") +:   ((Rs "d") +:   L0)
          outerSek = sek1 ::: sek2
          dictPart = lem (((Rs "e") /\ (Rs "f")) : Nil) <+ ((Rs "g") /\ (Rs "i"))
          input = outerSek +: dictPart  -- Prepend outerSek to Dict creates Sekdict
          Tuple sekdictKid (Tuple _ kube) = runState (addM input) (Tuple (Kid 0) emptyKube)
        -- Check if it's actually a Sekdict
        case input of
          Sekdict _ _ -> true `shouldEqual` true
          _ -> false `shouldEqual` true
        -- roots should contain exactly one Kid (the Sekdict)
        Set.size kube.roots `shouldEqual` 1
        Set.member sekdictKid kube.roots `shouldEqual` true
        -- seqs should have 2 positions
        Array.length kube.seqs `shouldEqual` 2
        let bi0 = getBiAt 0 kube.seqs
        let bi1 = getBiAt 1 kube.seqs
        -- Find inner Sek Kids
        let innerKid1Set = getValues bi0 (Rs "a")
        let innerKid2Set = getValues bi0 (Rs "c")
        Set.size innerKid1Set `shouldEqual` 1
        Set.size innerKid2Set `shouldEqual` 1
        let innerKid1 = fromMaybe (Kid (-1)) (Set.findMin innerKid1Set)
        let innerKid2 = fromMaybe (Kid (-1)) (Set.findMin innerKid2Set)
        -- The two inner Seks should be different Kids
        (innerKid1 /= innerKid2) `shouldEqual` true
        -- "a" and "b" should map to the same inner Sek Kid
        hasMappingValue (Rs "a") innerKid1 bi0 `shouldEqual` true
        hasMappingValue (Rs "b") innerKid1 bi1 `shouldEqual` true
        -- "c" and "d" should map to the same inner Sek Kid
        hasMappingValue (Rs "c") innerKid2 bi0 `shouldEqual` true
        hasMappingValue (Rs "d") innerKid2 bi1 `shouldEqual` true
        -- Both inner Seks should reference the Sekdict
        (Array.length kube.refSeqs >= 2) `shouldEqual` true
        let refBi0 = getBiAt 0 kube.refSeqs
        let refBi1 = getBiAt 1 kube.refSeqs
        hasMappingValue innerKid1 sekdictKid refBi0 `shouldEqual` true
        hasMappingValue innerKid2 sekdictKid refBi1 `shouldEqual` true
        -- Find dict pair Kids
        let pairKid1Set = getValues kube.keys (Rs "e")
        let pairKid2Set = getValues kube.keys (Rs "g")
        Set.size pairKid1Set `shouldEqual` 1
        Set.size pairKid2Set `shouldEqual` 1
        let pairKid1 = fromMaybe (Kid (-1)) (Set.findMin pairKid1Set)
        let pairKid2 = fromMaybe (Kid (-1)) (Set.findMin pairKid2Set)
        -- The two pairs should be different Kids
        (pairKid1 /= pairKid2) `shouldEqual` true
        -- "e" and "f" should map to the same pair Kid
        hasMappingValue (Rs "e") pairKid1 kube.keys `shouldEqual` true
        hasMappingValue (Rs "f") pairKid1 kube.vals `shouldEqual` true
        -- "g" and "i" should map to the same pair Kid
        hasMappingValue (Rs "g") pairKid2 kube.keys `shouldEqual` true
        hasMappingValue (Rs "i") pairKid2 kube.vals `shouldEqual` true
        -- Both pairs should reference the Sekdict
        hasMappingValue pairKid1 sekdictKid kube.refKeys `shouldEqual` true
        hasMappingValue pairKid2 sekdictKid kube.refKeys `shouldEqual` true

    describe "Example 10: Store 2 distinct Lems" do
      it "Sek a b, Sek c d -> {seqs = [{a -> k0, c -> k1}, {b -> k0, d -> k1}], roots = {k0,k1}}" do
        let
          lem1 = (Rs "a") +:   ((Rs "b") +:   L0)
          lem2 = (Rs "c") +:   ((Rs "d") +:   L0)
          Tuple k0 (Tuple nextKid1 kube1) = runState (addM lem1) (Tuple (Kid 0) emptyKube)
          Tuple k1 (Tuple _ kube2) = runState (addM lem2) (nextKid1 /\ kube1)
        -- The two Lems should get different Kids
        (k0 /= k1) `shouldEqual` true
        -- roots should contain both Kids
        Set.size kube2.roots `shouldEqual` 2
        Set.member k0 kube2.roots `shouldEqual` true
        Set.member k1 kube2.roots `shouldEqual` true
        -- seqs should have 2 positions
        Array.length kube2.seqs `shouldEqual` 2
        let bi0 = getBiAt 0 kube2.seqs
        let bi1 = getBiAt 1 kube2.seqs
        -- "a" and "b" should map to k0
        hasMappingValue (Rs "a") k0 bi0 `shouldEqual` true
        hasMappingValue (Rs "b") k0 bi1 `shouldEqual` true
        -- "c" and "d" should map to k1
        hasMappingValue (Rs "c") k1 bi0 `shouldEqual` true
        hasMappingValue (Rs "d") k1 bi1 `shouldEqual` true
