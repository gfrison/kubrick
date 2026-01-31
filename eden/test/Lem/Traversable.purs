module Test.Lem.Traversable where

import Prelude

import Data.Array as Array
import Data.List as List
import Data.List.Types (List(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse, sequence)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Kubrick.Lem (Lem(..), (:::), (<+>))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Lem.Traversable" do
    describe "traverse" do
      it "traverses L0" do
        traverse (\x -> Just (x + 1)) L0 `shouldEqual` Just L0

      it "traverses L1" do
        traverse (\x -> Just (x + 1)) (L1 5) `shouldEqual` Just (L1 6)

      it "traverses L1 with Nothing" do
        (traverse (\_ -> Nothing :: Maybe Int) (L1 5) :: Maybe (Lem Int)) `shouldEqual` Nothing

      it "traverses Pair" do
        let pair = Pair (L1 1) (L1 2)
        traverse (\x -> Just (x * 2)) pair `shouldEqual` Just (Pair (L1 2) (L1 4))

      it "traverses nested Pair" do
        let pair = Pair (Pair (L1 1) (L1 2)) (L1 3)
        traverse (\x -> Just (x + 10)) pair `shouldEqual` Just (Pair (Pair (L1 11) (L1 12)) (L1 13))

      it "traverses Sek" do
        let sek = Sek (L1 1) (L1 2) (List.fromFoldable [ L1 3, L1 4 ])
        let result = traverse (\x -> Just (x * 2)) sek
        result `shouldEqual` Just (Sek (L1 2) (L1 4) (List.fromFoldable [ L1 6, L1 8 ]))

      it "traverses Sek with empty rest" do
        let sek = Sek (L1 10) (L1 20) Nil
        traverse (\x -> Just (x / 10)) sek `shouldEqual` Just (Sek (L1 1) (L1 2) Nil)

      it "short-circuits on Nothing in Sek" do
        let sek = Sek (L1 1) (L1 2) (List.singleton (L1 3))
        let result = traverse (\x -> if x == 2 then Nothing else Just x) sek :: Maybe (Lem Int)
        result `shouldEqual` Nothing

      it "traverses Bag" do
        let bag = Bag (L1 1) (L1 2) (List.fromFoldable [ L1 3, L1 4 ])
        let result = traverse (\x -> Just (x + 100)) bag
        result `shouldEqual` Just (Bag (L1 101) (L1 102) (List.fromFoldable [ L1 103, L1 104 ]))

      it "traverses Bag with empty rest" do
        let bag = Bag (L1 5) (L1 10) Nil
        traverse (\x -> Just (x * 3)) bag `shouldEqual` Just (Bag (L1 15) (L1 30) Nil)

      it "traverses Choice" do
        let choice = Choice (L1 1) (L1 2) (List.singleton (L1 3))
        let result = traverse (\x -> Just (x + 5)) choice
        result `shouldEqual` Just (Choice (L1 6) (L1 7) (List.singleton (L1 8)))

      it "traverses Dict" do
        let dict = Dict (L1 1 /\ L1 2) (L1 3 /\ L1 4) Nil
        let result = traverse (\x -> Just (x * 10)) dict
        result `shouldEqual` Just (Dict (L1 10 /\ L1 20) (L1 30 /\ L1 40) Nil)

      it "traverses Dict with array" do
        let dict = Dict (L1 1 /\ L1 2) (L1 3 /\ L1 4) (List.singleton (L1 5 /\ L1 6))
        let result = traverse (\x -> Just (x + 1)) dict
        result `shouldEqual` Just (Dict (L1 2 /\ L1 3) (L1 4 /\ L1 5) (List.singleton (L1 6 /\ L1 7)))

      it "traverses Sekdict with S1" do
        let sekdict = L1 10 ::: Pair (L1 1) (L1 2) -- Creates Sekdict (S1 (L1 10)) (D1 (L1 1) (L1 2))
        let result = traverse (\x -> Just (x + 5)) sekdict
        result `shouldEqual` Just (L1 15 ::: Pair (L1 6) (L1 7))

      it "traverses Sekdict with S2" do
        let sekdict = Sek (L1 1) (L1 2) (List.singleton (L1 3)) ::: Pair (L1 4) (L1 5)
        let result = traverse (\x -> Just (x * 2)) sekdict
        result `shouldEqual` Just (Sek (L1 2) (L1 4) (List.singleton (L1 6)) ::: Pair (L1 8) (L1 10))

      it "traverses Sekdict with D2" do
        let sekdict = L1 1 ::: Dict (L1 2 /\ L1 3) (L1 4 /\ L1 5) Nil -- Sekdict (S1 (L1 1)) (D2 ...)
        let result = traverse (\x -> Just (x + 10)) sekdict
        result `shouldEqual` Just (L1 11 ::: Dict (L1 12 /\ L1 13) (L1 14 /\ L1 15) Nil)

      it "traverses Bagdict with B1" do
        let bagdict = Pair (L1 1) (L1 2) <+> L1 5 -- Bagdict (B1 (L1 5)) (D1 (L1 1) (L1 2))
        let result = traverse (\x -> Just (x * 3)) bagdict
        result `shouldEqual` Just (Pair (L1 3) (L1 6) <+> L1 15)

      it "traverses Bagdict with B2" do
        let bagdict = Pair (L1 4) (L1 5) <+> Bag (L1 1) (L1 2) (List.singleton (L1 3))
        let result = traverse (\x -> Just (x + 100)) bagdict
        result `shouldEqual` Just (Pair (L1 104) (L1 105) <+> Bag (L1 101) (L1 102) (List.singleton (L1 103)))

      it "preserves structure with identity function" do
        let lem = Sek (L1 1) (Pair (L1 2) (L1 3)) (List.singleton (L1 4))
        traverse (\x -> Just x) lem `shouldEqual` Just lem

    describe "sequence" do
      it "sequences L0" do
        (sequence L0 :: Maybe (Lem Int)) `shouldEqual` Just L0

      it "sequences L1 with Just" do
        sequence (L1 (Just 42)) `shouldEqual` Just (L1 42)

      it "sequences L1 with Nothing" do
        (sequence (L1 Nothing) :: Maybe (Lem Int)) `shouldEqual` Nothing

      it "sequences Pair" do
        let pair = Pair (L1 (Just 1)) (L1 (Just 2))
        sequence pair `shouldEqual` Just (Pair (L1 1) (L1 2))

      it "sequences Pair with Nothing" do
        let pair = Pair (L1 (Just 1)) (L1 Nothing) :: Lem (Maybe Int)
        sequence pair `shouldEqual` Nothing

      it "sequences Sek" do
        let sek = Sek (L1 (Just 1)) (L1 (Just 2)) (List.singleton (L1 (Just 3)))
        sequence sek `shouldEqual` Just (Sek (L1 1) (L1 2) (List.singleton (L1 3)))

      it "sequences Sek with Nothing" do
        let sek = Sek (L1 (Just 1)) (L1 Nothing) Nil :: Lem (Maybe Int)
        sequence sek `shouldEqual` Nothing

      it "sequences Bag" do
        let bag = Bag (L1 (Just 10)) (L1 (Just 20)) (List.singleton (L1 (Just 30)))
        sequence bag `shouldEqual` Just (Bag (L1 10) (L1 20) (List.singleton (L1 30)))

      it "sequences Choice" do
        let choice = Choice (L1 (Just 5)) (L1 (Just 10)) Nil
        sequence choice `shouldEqual` Just (Choice (L1 5) (L1 10) Nil)

      it "sequences Dict" do
        let dict = Dict (Tuple (L1 (Just 1)) (L1 (Just 2))) (Tuple (L1 (Just 3)) (L1 (Just 4))) Nil
        sequence dict `shouldEqual` Just (Dict (L1 1 /\ L1 2) (L1 3 /\ L1 4) Nil)

      it "sequences Sekdict" do
        let sekdict = L1 (Just 5) ::: Pair (L1 (Just 1)) (L1 (Just 2))
        sequence sekdict `shouldEqual` Just (L1 5 ::: Pair (L1 1) (L1 2))

      it "sequences Bagdict" do
        let bagdict = Pair (L1 (Just 20)) (L1 (Just 30)) <+> L1 (Just 10)
        sequence bagdict `shouldEqual` Just (Pair (L1 20) (L1 30) <+> L1 10)

      it "sequences complex nested structure" do
        let lem = Sek (Pair (L1 (Just 1)) (L1 (Just 2))) (L1 (Just 3)) (List.singleton (L1 (Just 4)))
        sequence lem `shouldEqual` Just (Sek (Pair (L1 1) (L1 2)) (L1 3) (List.singleton (L1 4)))

    describe "traverse and sequence relationship" do
      it "sequence = traverse identity" do
        let lem = L1 (Just 42)
        sequence lem `shouldEqual` traverse identity lem

      it "sequence = traverse identity for Pair" do
        let lem = Pair (L1 (Just 1)) (L1 (Just 2))
        sequence lem `shouldEqual` traverse identity lem

      it "sequence = traverse identity for Sek" do
        let lem = Sek (L1 (Just 1)) (L1 (Just 2)) Nil
        sequence lem `shouldEqual` traverse identity lem

    describe "traverse with Array" do
      it "collects results into Array for L1" do
        let f x = [ x, x + 1, x + 2 ]
        let result = traverse f (L1 5)
        Array.length result `shouldEqual` 3
        result `shouldEqual` [ L1 5, L1 6, L1 7 ]

      it "combines results for Pair" do
        let f x = [ x, x * 2 ]
        let result = traverse f (Pair (L1 1) (L1 2))
        Array.length result `shouldEqual` 4
        result `shouldEqual`
          [ Pair (L1 1) (L1 2)
          , Pair (L1 1) (L1 4)
          , Pair (L1 2) (L1 2)
          , Pair (L1 2) (L1 4)
          ]

    describe "traverse maintains Ord constraint" do
      it "works with Bag requiring Ord" do
        let bag = Bag (L1 1) (L1 2) (List.singleton (L1 3))
        let result = traverse (\x -> Just (x + 1)) bag
        result `shouldEqual` Just (Bag (L1 2) (L1 3) (List.singleton (L1 4)))
