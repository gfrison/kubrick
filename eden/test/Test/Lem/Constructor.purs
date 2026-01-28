module Test.Lem.Constructor where

import Prelude

import Data.List ((:))
import Data.List.Types (List(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Kubrick.Lem (Lem(..), lem, (:::), (<+>), (+:))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "lem constructor" do
    describe "lem for single value in list" do
      it "lem (1 : Nil) => L1 1" do
        let result = lem (1 : Nil)
        result `shouldEqual` L1 1

      it "lem (\"hello\" : Nil) => L1 \"hello\"" do
        let result = lem ("hello" : Nil)
        result `shouldEqual` L1 "hello"

    describe "lem for sequences (List)" do
      -- it "seks of seks" do
      --  let result = lem(1:2:Nil) ::: lem(3:4:Nil)
      --  result `shouldEqual` Sek (Sek (L1 1) (L1 2) Nil) (Sek (L1 3) (L1 4) Nil) Nil
      it "lem (1 : 2 : 3 : Nil) => Sek" do
        let result = lem (1 : 2 : 3 : Nil)
        result `shouldEqual` Sek (L1 1) (L1 2) (L1 3 : Nil)

      it "lem (1 : 2 : Nil) => Sek with two elements" do
        let result = lem (1 : 2 : Nil)
        result `shouldEqual` Sek (L1 1) (L1 2) Nil

      it "lem (1 : Nil) => L1 1 (single element collapses)" do
        let result = lem (1 : Nil)
        result `shouldEqual` L1 1

      it "lem Nil => L0" do
        let result = lem (Nil :: List Int) :: Lem Int
        result `shouldEqual` L0

    describe "lem for dictionaries (List of Tuples)" do
      it "lem ((1 /\\ 2) : (3 /\\ 4) : Nil) => Dict" do
        let result = lem ((1 /\ 2) : (3 /\ 4) : Nil)
        result `shouldEqual` Dict (Tuple (L1 1) (L1 2)) (Tuple (L1 3) (L1 4)) Nil

      it "lem ((1 /\\ 2) : (3 /\\ 4) : (5 /\\ 6) : Nil) => Dict with 3 pairs" do
        let result = lem ((1 /\ 2) : (3 /\ 4) : (5 /\ 6) : Nil)
        result `shouldEqual` Dict (Tuple (L1 1) (L1 2)) (Tuple (L1 3) (L1 4)) (Tuple (L1 5) (L1 6) : Nil)

      it "lem ((1 /\\ 2) : Nil) => Pair" do
        let result = lem ((1 /\ 2) : Nil)
        result `shouldEqual` Pair (L1 1) (L1 2)

      it "lem (Nil :: List (Tuple Int Int)) => L0" do
        let result = lem (Nil :: List (Tuple Int Int)) :: Lem Int
        result `shouldEqual` L0

    describe "lem with duplicate keys (uses dict smart constructor)" do
      it "removes duplicate keys" do
        let result = lem ((1 /\ 10) : (2 /\ 20) : (1 /\ 30) : Nil)
        result `shouldEqual` Dict (Tuple (L1 1) (L1 10)) (Tuple (L1 2) (L1 20)) Nil

    describe "combining lem results to create Sekdict" do
      it "lem (1 : 2 : Nil) ::: lem ((3 /\\ 4) : Nil) => Sekdict" do
        let sek = lem (1 : 2 : Nil)
        let dict = lem ((3 /\ 4) : Nil)
        let result = sek ::: dict
        result `shouldEqual` (Sek (L1 1) (L1 2) Nil ::: Pair (L1 3) (L1 4))

    describe "combining lem results to create Bagdict" do
      it "lem ((1 /\\ 2) : Nil) <+> lem (3 : 4 : Nil) => Bagdict" do
        let dict = lem ((1 /\ 2) : Nil)
        let bag = lem (3 : 4 : Nil)
        let result = dict <+> bag
        result `shouldEqual` (Pair (L1 1) (L1 2) <+> Sek (L1 3) (L1 4) Nil)
