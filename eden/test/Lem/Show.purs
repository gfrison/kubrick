module Test.Lem.Show where

import Prelude

import Data.List as List
import Data.List.Types (List(..))
import Data.String as String
import Data.Tuple.Nested ((/\))
import Kubrick.Lem (Lem(..), (<+), (<+>), (+:))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

spec :: Spec Unit
spec = do
  describe "Lem.Show" do
    describe "showLem" do
      it "shows L0" do
        show (L0 :: Lem Int) `shouldEqual` "L0"

      it "shows L1" do
        show (L1 42) `shouldEqual` "(L1 42)"

      it "shows Pair" do
        show (Pair (L1 1) (L1 2)) `shouldEqual` "(Pair (L1 1) (L1 2))"

      it "shows Sek with empty rest" do
        show (Sek (L1 1) (L1 2) Nil) `shouldEqual` "(Sek (L1 1) (L1 2) Nil)"

      it "shows Sek with rest" do
        show (Sek (L1 1) (L1 2) (List.singleton (L1 3))) `shouldEqual` "(Sek (L1 1) (L1 2) ((L1 3) : Nil))"

      it "shows Bag with empty rest" do
        show (Bag (L1 1) (L1 2) Nil) `shouldEqual` "(Bag (L1 1) (L1 2) Nil)"

      it "shows Bag with rest" do
        show (Bag (L1 1) (L1 2) (List.singleton (L1 3))) `shouldEqual` "(Bag (L1 1) (L1 2) ((L1 3) : Nil))"

      it "shows Choice with empty rest" do
        show (Choice (L1 1) (L1 2) Nil) `shouldEqual` "(Choice (L1 1) (L1 2) Nil)"

      it "shows Choice with rest" do
        show (Choice (L1 1) (L1 2) (List.singleton (L1 3))) `shouldEqual` "(Choice (L1 1) (L1 2) ((L1 3) : Nil))"

      it "shows Dict with empty rest" do
        show (Dict (L1 1 /\ L1 10) (L1 2 /\ L1 20) Nil) `shouldEqual` "(Dict (Tuple (L1 1) (L1 10)) (Tuple (L1 2) (L1 20)) Nil)"

      it "shows Dict with rest" do
        show (Dict (L1 1 /\ L1 10) (L1 2 /\ L1 20) (List.singleton (L1 3 /\ L1 30))) `shouldEqual` "(Dict (Tuple (L1 1) (L1 10)) (Tuple (L1 2) (L1 20)) ((Tuple (L1 3) (L1 30)) : Nil))"

      it "shows nested Pair" do
        show (Pair (Pair (L1 1) (L1 2)) (L1 3)) `shouldEqual` "(Pair (Pair (L1 1) (L1 2)) (L1 3))"

    describe "show Lem types created with public API" do
      it "shows L0" do
        show (L0 :: Lem Int) `shouldEqual` "L0"

      it "shows L1" do
        show (L1 42 :: Lem Int) `shouldEqual` "(L1 42)"

      it "shows Sek created with <+ operator" do
        let sek = ((L1 1) :: Lem Int) <+ 2 :: Lem Int
        -- Sek internally contains S2, verify output contains "Sek"
        show sek `shouldSatisfy` (String.contains (String.Pattern "Sek"))

      it "shows Bag created with <+> operator" do
        let bag = (L1 1 :: Lem Int) <+> (L1 2)
        show bag `shouldSatisfy` (String.contains (String.Pattern "Bag"))

      it "shows Dict created with smart constructor" do
        let dict = (Dict (L1 1 /\ L1 10) (L1 2 /\ L1 20) Nil) :: Lem Int
        show dict `shouldSatisfy` (String.contains (String.Pattern "Dict"))

    describe "showLem with composite types" do
      it "shows Sekdict" do
        let sekdict = (1 /\ 2) +: L1 3
        show sekdict `shouldSatisfy` (String.contains (String.Pattern "Sekdict"))

      it "shows Bagdict" do
        let bagdict = Pair (L1 2) (L1 3) <+> L1 1
        show bagdict `shouldSatisfy` (String.contains (String.Pattern "Bagdict"))
