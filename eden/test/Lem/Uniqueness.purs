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
      -- <+> now creates flat Bags
      let bag = L1 1 <+> L1 2 <+> L1 3
      bag `shouldEqual` Bag (L1 1) (L1 2) (Cons (L1 3) Nil)

    it "removes duplicates and collapses to L1 when only one unique element" do
      let bag = L1 1 <+> L1 1 <+> L1 1
      bag `shouldEqual` L1 1

    it "removes duplicates and keeps two unique elements" do
      -- <+> now creates flat Bags with uniqueness
      let bag = L1 1 <+> L1 2 <+> L1 1 <+> L1 2
      bag `shouldEqual` Bag (L1 1) (L1 2) Nil

    it "removes duplicates from mixed positions" do
      -- <+> now creates flat Bags with uniqueness
      let bag = L1 1 <+> L1 2 <+> L1 2 <+> L1 3
      bag `shouldEqual` Bag (L1 1) (L1 2) (Cons (L1 3) Nil)

  describe "choice uniqueness" do
    it "preserves all unique elements" do
      -- \/ creates flattened Choices
      let choice = (L1 "a" :: Lem String) \/ (L1 "b" :: Lem String) \/ (L1 "c" :: Lem String)
      choice `shouldEqual` Choice (L1 "a") (L1 "b") (Cons (L1 "c") Nil)

    it "removes duplicates and collapses to L1 when only one unique element" do
      let choice = (L1 "a" :: Lem String) \/ (L1 "a" :: Lem String)
      choice `shouldEqual` L1 "a"

    it "removes duplicates and keeps two unique elements" do
      -- \/ creates flattened Choices with duplicates removed
      let choice = (L1 "x" :: Lem String) \/ (L1 "y" :: Lem String) \/ (L1 "x" :: Lem String) \/ (L1 "y" :: Lem String)
      choice `shouldEqual` Choice (L1 "x") (L1 "y") Nil
