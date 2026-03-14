module Test.Lem.Show where

import Prelude

import Data.List as List
import Data.List.Types (List(..))
import Data.Tuple.Nested ((/\))
import Kubrick.Lem (Lem(..), (<+), (<+>), (+:))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Data.String as String

spec :: Spec Unit
spec = do
  describe "Lem.Show" do
    describe "showLem" do
      it "shows L0" do
        show (L0 :: Lem Int) `shouldEqual` ""

      it "shows L1" do
        show (L1 42) `shouldEqual` "42"

      it "shows Pair" do
        show (Pair (L1 1) (L1 2)) `shouldEqual` "1 -> 2"

      it "shows Sek with empty rest" do
        show (Sek (L1 1) (L1 2) Nil) `shouldEqual` "[1 2]"

      it "shows Sek with rest" do
        show (Sek (L1 1) (L1 2) (List.singleton (L1 3))) `shouldEqual` "[1 2 3]"

      it "shows Bag with empty rest" do
        show (Bag (L1 1) (L1 2) Nil) `shouldEqual` "{1 2}"

      it "shows Bag with rest" do
        show (Bag (L1 1) (L1 2) (List.singleton (L1 3))) `shouldEqual` "{1 2 3}"

      it "shows Choice with empty rest" do
        show (Choice (L1 1) (L1 2) Nil) `shouldEqual` "(1;2)"

      it "shows Choice with rest" do
        show (Choice (L1 1) (L1 2) (List.singleton (L1 3))) `shouldEqual` "(1;2;3)"

      it "shows Dict with empty rest" do
        show (Dict (L1 1 /\ L1 10) (L1 2 /\ L1 20) Nil) `shouldEqual` "1 -> 10 2 -> 20"

      it "shows Dict with rest" do
        show (Dict (L1 1 /\ L1 10) (L1 2 /\ L1 20) (List.singleton (L1 3 /\ L1 30))) `shouldEqual` "1 -> 10 2 -> 20 3 -> 30"

      it "shows nested Pair" do
        show (Pair (Pair (L1 1) (L1 2)) (L1 3)) `shouldEqual` "1 -> 2 -> 3"

    describe "show Lem types created with public API" do
      it "shows L0" do
        show (L0 :: Lem Int) `shouldEqual` ""

      it "shows L1" do
        show (L1 42 :: Lem Int) `shouldEqual` "42"

      it "shows Bag created with <+ operator" do
        let bag = 2 <+ ((L1 1) :: Lem Int) :: Lem Int
        show bag `shouldSatisfy` (String.contains (String.Pattern "{"))

      it "shows Bag created with <+> operator" do
        let bag = (L1 1 :: Lem Int) <+> (L1 2)
        show bag `shouldSatisfy` (String.contains (String.Pattern "{"))

      it "shows Dict created with smart constructor" do
        let d = (Dict (L1 1 /\ L1 10) (L1 2 /\ L1 20) Nil) :: Lem Int
        show d `shouldSatisfy` (String.contains (String.Pattern "->"))

    describe "showLem with composite types" do
      it "shows Sekdict" do
        let sekdict = (1 /\ 2) +: L1 3
        show sekdict `shouldSatisfy` (String.contains (String.Pattern "->"))

      it "shows Bagdict" do
        let bagdict = Pair (L1 2) (L1 3) <+> L1 1
        show bagdict `shouldSatisfy` (String.contains (String.Pattern "{"))
