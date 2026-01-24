module Test.Lem.Uniqueness where

import Prelude

import Data.List ((:))
import Data.List.Types (List(..))
import Lem (Lem(..))
import Lem as Lem
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "bag uniqueness" do
    it "preserves all unique elements" do
      let bag = Lem.bag 1 2 (3 : Nil)
      bag `shouldEqual` Bag (L1 1) (L1 2) (L1 3 : Nil)

    it "removes duplicates and collapses to L1 when only one unique element" do
      let bag = Lem.bag 1 1 (1 : Nil)
      bag `shouldEqual` L1 1

    it "removes duplicates and keeps two unique elements" do
      let bag = Lem.bag 1 2 (1 : 2 : Nil)
      bag `shouldEqual` Bag (L1 1) (L1 2) Nil

    it "removes duplicates from mixed positions" do
      let bag = Lem.bag 1 2 (2 : 3 : Nil)
      bag `shouldEqual` Bag (L1 1) (L1 2) (L1 3 : Nil)

  describe "choice uniqueness" do
    it "preserves all unique elements" do
      let choice = Lem.choice "a" "b" ("c" : Nil)
      choice `shouldEqual` Choice (L1 "a") (L1 "b") (L1 "c" : Nil)

    it "removes duplicates and collapses to L1 when only one unique element" do
      let choice = Lem.choice "a" "a" Nil
      choice `shouldEqual` L1 "a"

    it "removes duplicates and keeps two unique elements" do
      let choice = Lem.choice "x" "y" ("x" : "y" : Nil)
      choice `shouldEqual` Choice (L1 "x") (L1 "y") Nil
