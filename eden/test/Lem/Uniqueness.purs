module Test.Lem.Uniqueness where

import Prelude

import Data.List.Types (List(..))
import Kubrick.Lem (Lem(..), (<+>), (\/))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "bag uniqueness" do
    it "preserves all unique elements" do
      -- <+> creates nested Bags
      let bag = L1 1 <+> L1 2 <+> L1 3
      bag `shouldEqual` Bag (Bag (L1 1) (L1 2) Nil) (L1 3) Nil

    it "removes duplicates and collapses to L1 when only one unique element" do
      let bag = L1 1 <+> L1 1 <+> L1 1
      bag `shouldEqual` L1 1

    it "removes duplicates and keeps two unique elements" do
      -- <+> creates nested Bags
      let bag = L1 1 <+> L1 2 <+> L1 1 <+> L1 2
      bag `shouldEqual` Bag (Bag (Bag (L1 1) (L1 2) Nil) (L1 1) Nil) (L1 2) Nil

    it "removes duplicates from mixed positions" do
      -- <+> creates nested Bags
      let bag = L1 1 <+> L1 2 <+> L1 2 <+> L1 3
      bag `shouldEqual` Bag (Bag (Bag (L1 1) (L1 2) Nil) (L1 2) Nil) (L1 3) Nil

  describe "choice uniqueness" do
    it "preserves all unique elements" do
      -- \/ creates nested Choices
      let choice = L1 "a" \/ L1 "b" \/ L1 "c"
      choice `shouldEqual` Choice (Choice (L1 "a") (L1 "b") Nil) (L1 "c") Nil

    it "removes duplicates and collapses to L1 when only one unique element" do
      let choice = L1 "a" \/ L1 "a"
      choice `shouldEqual` L1 "a"

    it "removes duplicates and keeps two unique elements" do
      -- \/ creates nested Choices
      let choice = L1 "x" \/ L1 "y" \/ L1 "x" \/ L1 "y"
      choice `shouldEqual` Choice (Choice (Choice (L1 "x") (L1 "y") Nil) (L1 "x") Nil) (L1 "y") Nil
