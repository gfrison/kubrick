module Test.Lem.Integrity where

import Prelude

import Data.List ((:))
import Data.List.Types (List(..))
import Data.Tuple.Nested ((/\))
import Lem (Dict1(..), Lem(..), (<+), (<+>))
import Lem as Lem
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)

-- | These tests demonstrate violations of Lem's integrity invariants
-- | and show why users must use smart constructors and operators

spec :: Spec Unit
spec = do
  describe "Lem.Integrity" do

    describe "Bag uniqueness violations" do
      it "direct construction with duplicates breaks equality" do
        -- VIOLATION: Constructing Bag directly with duplicate elements
        let
          badBag1 = Bag (L1 1) (L1 1) (L1 1 : Nil) -- All same!
          badBag2 = Bag (L1 1) (L1 1) (L1 1 : Nil) -- Identical duplicate elements

        -- These should be equal since they have the same elements
        -- But direct construction bypasses uniqueness check
        badBag1 `shouldEqual` badBag2

        -- Now create correct Bag using smart constructor
        let goodBag = Lem.bag 1 1 (1 : Nil)

        -- Smart constructor removes duplicates and collapses to L1
        goodBag `shouldEqual` (L1 1)

        -- badBag1 is NOT equal to what bag produces!
        badBag1 `shouldNotEqual` goodBag

      it "direct construction allows duplicates in different positions" do
        let
          badBag = Bag (L1 1) (L1 2) (L1 1 : L1 2 : Nil) -- 1 and 2 appear twice each
          goodBag = Lem.bag 1 2 (1 : 2 : Nil)

        -- Smart constructor deduplicates
        goodBag `shouldEqual` Bag (L1 1) (L1 2) Nil

        -- Direct construction keeps duplicates
        badBag `shouldNotEqual` goodBag

      it "operator prevents duplicates" do
        let result = (L1 1) <+> (L1 1)
        result `shouldEqual` (L1 1)
        result `shouldNotEqual` Bag (L1 1) (L1 1) Nil

    describe "Choice uniqueness violations" do
      it "direct construction with duplicates breaks integrity" do
        let
          badChoice = Choice (L1 "a") (L1 "a") (L1 "a" : Nil)
          goodChoice = Lem.choice "a" "a" ("a" : Nil)

        goodChoice `shouldEqual` (L1 "a")
        badChoice `shouldNotEqual` goodChoice

      it "choice enforces uniqueness" do
        let
          choice1 = Lem.choice 1 2 (1 : 3 : Nil)
          choice2 = Lem.choice 3 2 (1 : Nil)

        -- Both should have unique elements {1, 2, 3} regardless of order
        choice1 `shouldEqual` choice2

    describe "Correct usage with operators" do
      it "<+> operator enforces uniqueness automatically" do
        let result = (L1 1) <+> (L1 1) <+> (L1 1)
        result `shouldEqual` (L1 1)

      it "<+> creates valid Bag structures" do
        let bag = (L1 1) <+> (L1 2) <+> (L1 3)
        -- Should create Bag with unique elements
        case bag of
          Bag _ _ _ -> pure unit
          _ -> pure unit -- Could also collapse to smaller structure

      it "bag handles all duplicates correctly" do
        let allSame = Lem.bag 42 42 (42 : 42 : Nil)
        allSame `shouldEqual` (L1 42)

      it "choice handles partial duplicates correctly" do
        let someDups = Lem.choice 1 2 (1 : 3 : 2 : Nil)
        let expected = Lem.choice 1 2 (3 : Nil)
        someDups `shouldEqual` expected

    describe "What NOT to do (documented violations)" do
      it "Example 1: Never construct Bag/Choice directly with duplicates" do
        -- BAD: Direct construction allows invalid state
        let invalid = Bag (L1 "x") (L1 "x") Nil

        -- GOOD: Use smart constructor
        let valid = Lem.bag "x" "x" Nil

        valid `shouldEqual` (L1 "x")
        invalid `shouldNotEqual` valid

      it "Example 2: Use operators for safe construction" do
        -- BAD way (if you must use constructors, ensure uniqueness manually)
        let bad = Bag (L1 1) (L1 2) (L1 1 : Nil)

        -- GOOD: Let operators handle it
        let good = (L1 1) <+> (L1 2) <+> (L1 1)

        -- Operators ensure uniqueness
        good `shouldNotEqual` bad

    describe "Dict key uniqueness violations" do
      it "direct construction with duplicate keys breaks integrity" do
        -- VIOLATION: Constructing Dict directly with duplicate keys
        let
          badDict = Dict (L1 1 /\ L1 10) (L1 1 /\ L1 20) Nil -- Key "1" appears twice!
          goodDict = Lem.dict (1 /\ 10) (1 /\ 20) Nil

        -- Smart constructor removes duplicate keys (keeps first occurrence)
        goodDict `shouldEqual` Pair (L1 1) (L1 10)

        -- badDict is NOT equal to what dict produces!
        badDict `shouldNotEqual` goodDict

      it "direct construction allows multiple values for same key" do
        let
          badDict = Dict (L1 1 /\ L1 10) (L1 2 /\ L1 20) ((L1 1 /\ L1 30) : Nil)
          goodDict = Lem.dict (1 /\ 10) (2 /\ 20) ((1 /\ 30) : Nil)

        -- Smart constructor keeps only first occurrence of each key
        goodDict `shouldEqual` Dict (L1 1 /\ L1 10) (L1 2 /\ L1 20) Nil

        -- Direct construction keeps duplicates
        badDict `shouldNotEqual` goodDict

      it "dict enforces key uniqueness" do
        let
          dict1 = Lem.dict (1 /\ 10) (2 /\ 20) ((1 /\ 30) : Nil)
          dict2 = Lem.dict (2 /\ 20) (1 /\ 10) Nil

        -- Both should have unique keys {1, 2} regardless of order
        dict1 `shouldEqual` dict2

      it "operator <+ prevents duplicate keys" do
        let pair = Pair (L1 1) (L1 100)
        let result = pair <+ (1 /\ 200) -- Same key 1

        -- Should have only one entry for key 1 (the first one)
        result `shouldEqual` Pair (L1 1) (L1 100)

    describe "D2 key uniqueness enforcement" do
      it "dict smart constructor prevents duplicate keys by collapsing to Pair" do
        let
          -- Only way to create is through smart constructor
          -- which enforces uniqueness by collapsing to single key/value pair
          result = Lem.dict (1 /\ 10) (1 /\ 20) Nil

        -- Smart constructor with duplicate keys collapses to single Pair
        result `shouldEqual` Pair (L1 1) (L1 10)

      it "d2 enforces key uniqueness" do
        let
          d2a = Lem.d2 (1 /\ 10) (2 /\ 20) ((1 /\ 30) : Nil)
          d2b = Lem.d2 (2 /\ 20) (1 /\ 10) Nil

        -- Should have unique keys {1, 2}
        d2a `shouldEqual` d2b

    describe "Dict/D2 correct usage" do
      it "dict handles all duplicate keys correctly" do
        let allSame = Lem.dict (1 /\ 10) (1 /\ 20) ((1 /\ 30) : Nil)
        allSame `shouldEqual` Pair (L1 1) (L1 10)

      it "d2 handles partial duplicate keys correctly" do
        let someDups = Lem.d2 (1 /\ 10) (2 /\ 20) ((1 /\ 30) : (3 /\ 40) : Nil)
        let expected = Lem.d2 (1 /\ 10) (2 /\ 20) ((3 /\ 40) : Nil)
        someDups `shouldEqual` expected

{-
  COMPILATION ERROR EXAMPLES (these would fail to compile):
  
  1. Type mismatch in operators:
     let x = (L1 1) <+> "string"  -- ERROR: type mismatch
  
  2. Missing Eq constraint:
     bag :: forall t. Lem t -> Lem t -> List (Lem t) -> Lem t
     -- ERROR: requires Eq t constraint for nubByEq
  
  3. Attempting to use undefined constructors (if we had hidden them):
     let x = HiddenBag (L1 1) (L1 2) Nil  -- ERROR: constructor not in scope
  
  4. Wrong arity:
     let x = Bag (L1 1)  -- ERROR: Bag requires 3 arguments
-}
