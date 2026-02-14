module Test.Lem.Constructor where

import Prelude

import Data.List ((:))
import Data.List.Types (List(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Kubrick.Lem (Lem(..), Sek1(..), Dict1(..), Bag1(..), lem, (:::), (<+>), (<+),(+>), (+:), (:+), (\/))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Kubrick.Types (Raw(..))

spec :: Spec Unit
spec = do
  describe "lem constructor" do
    describe "bag <+" do
      it "1 <+ 2 <+ L0 -> Bag (L1 1) (L1 2) Nil" do
        let result = 1 <+ 2 <+ L0
        result `shouldEqual` Bag (L1 1) (L1 2) Nil
      it "1 <+ 2 <+ 3 <+ L0 -> Bag (L1 1) (L1 2) (L1 3 : Nil)" do
        let result = 1 <+ 2 <+ 3 <+ L0
        result `shouldEqual` Bag (L1 1) (L1 2) (L1 3 : Nil)

      it "L0 +> 1 +> 2 -> Bag (L1 1) (L1 2) Nil" do
        let result = L0 +> 1 +> 2
        result `shouldEqual` Bag (L1 1) (L1 2) Nil

    describe "sek from +:" do
      it "Ri 1 +: Ri 2 +: Ri 3 -> Sek (L1 1) (L1 2) (L1 3 : Nil)" do
        let result = Ri 1 +: Ri 2 +: Ri 3 +: L0
        result `shouldEqual` Sek (L1 (Ri 1)) (L1 (Ri 2)) (L1 (Ri 3) : Nil)
      it "(Ri 1 +: L1 Ri 2) :+ Ri 3 -> Sek (L1 1) (L1 2) (L1 3 : Nil)" do
        let result = (Ri 1 +: L1 (Ri 2)) :+ Ri 3
        result `shouldEqual` Sek (L1 (Ri 1)) (L1 (Ri 2)) (L1 (Ri 3) : Nil)
      it "(Ri 1 +: Ri 2 +: L1 Ri 3) :+ Ri 4 :+ Ri 5 -> Sek (L1 1) (L1 2) (L1 3 : L1 4 : L1 5 : Nil)" do
        let result = (Ri 1 +: Ri 2 +: L1 (Ri 3)) :+ Ri 4 :+ Ri 5
        result `shouldEqual` Sek (L1 (Ri 1)) (L1 (Ri 2)) (L1 (Ri 3) : L1 (Ri 4) : L1 (Ri 5) : Nil)
      it "(Ri 1 +: Ri 2 +: L0) ::: (L0 :+ Ri 3 :+ Ri 4) -> Sek (Sek (L1 1) (L1 2) Nil) (Sek (L1 3) (L1 4) Nil) Nil" do
        let result = (Ri 1 +: Ri 2 +: L0) ::: (L0 :+ Ri 3 :+ Ri 4)
        result `shouldEqual` Sek (Sek (L1 (Ri 1)) (L1 (Ri 2)) Nil) (Sek (L1 (Ri 3)) (L1 (Ri 4)) Nil) Nil
      it "1 +: L1 2 -> Sek (L1 1) (L1 2) Nil" do
        let result = 1 +: L1 (2 :: Int)
        result `shouldEqual` Sek (L1 1) (L1 2) Nil
      it "1 +: 2 +: L0 with right assoc -> Sek (L1 1) (L1 2) Nil" do
        let result = 1 +: 2 +: L0
        result `shouldEqual` Sek (L1 1) (L1 2) Nil
      it "L0 :+ 1 :+ 2 with left assoc -> Sek (L1 1) (L1 2) Nil" do
        let result = L0 :+ 1 :+ 2
        result `shouldEqual` Sek (L1 1) (L1 2) Nil

      it "(Sek (L1 1) (L1 2) Nil) <+ 3 -> combines to Bag" do
        let result = 3 <+ (1 +: L1 (2 :: Int))
        result `shouldEqual` Bag (L1 3) (Sek (L1 1) (L1 2) Nil) Nil

    it "Pair (L1 a) (L1 b) <+ (c /\\ d) -> combines to Bagdict" do
        let result = ((-3) /\ (-4)) <+ lem (((-1) /\ (-2)) : Nil)
        result `shouldEqual` Dict (Tuple (L1 (-1)) (L1 (-2))) (Tuple (L1 (-3)) (L1 (-4))) Nil
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
  describe "Operator +: (prepend)" do
    it "1 +: L1 2 -> Sek (L1 1) (L1 2) Nil" do
      let result = 1 +: L1 (2 :: Int)
      result `shouldEqual` Sek (L1 1) (L1 2) Nil

    it "(1 /\\ 2) +: L1 3 -> Sekdict (S1 (L1 3)) (D1 (L1 1) (L1 2))" do
      let result = (1 /\ 2) +: L1 (3 :: Int)
      result `shouldEqual` Sekdict (S1 (L1 3)) (D1 (L1 1) (L1 2))

    it "(Sek (L1 2) (L1 3) Nil) +: L1 1 -> Sek (L1 1) (L1 2) (L1 3 : Nil)" do
      let result = (2 +: L1 (3 :: Int)) +: L1 (1 :: Int)
      result `shouldEqual` Sek (L1 1) (L1 2) (L1 3 : Nil)

    it "(Sek (L1 1) (L1 2) Nil) +: (Sek (L1 3) (L1 4) Nil) -> Sek (L1 1) (L1 2) (L1 3 : L1 4 : Nil)" do
      let result = (1 +: L1 (2 :: Int)) +: (3 +: L1 (4 :: Int))
      result `shouldEqual` Sek (L1 1) (L1 2) (L1 3 : L1 4 : Nil)

  describe "Operator ::: (creates Sek independently of arguments)" do
    it "(Sek (L1 1) (L1 2) Nil) ::: (Sek (L1 3) (L1 4) Nil) -> Sek (Sek (L1 1) (L1 2) Nil) (Sek (L1 3) (L1 4) Nil) Nil" do
      let result = (L1 (1 :: Int) ::: L1 (2 :: Int)) ::: (L1 (3 :: Int) ::: L1 (4 :: Int))
      result `shouldEqual` Sek (Sek (L1 1) (L1 2) Nil) (Sek (L1 3) (L1 4) Nil) Nil

    it "L1 1 ::: L1 2 -> Sek (L1 1) (L1 2) Nil" do
      let result = L1 (1 :: Int) ::: L1 (2 :: Int)
      result `shouldEqual` Sek (L1 1) (L1 2) Nil

    it "Pair (L1 a) (L1 b) ::: Sek (L1 1) (L1 2) Nil -> Sek with Pair and Sek" do
      let result = lem (((-1) /\ (-2)) : Nil) ::: (L1 (1 :: Int) ::: L1 (2 :: Int))
      result `shouldEqual` Sek (Pair (L1 (-1)) (L1 (-2))) (Sek (L1 1) (L1 2) Nil) Nil

    it "L1 1 ::: Pair (L1 a) (L1 b) -> Sek with L1 and Pair" do
      let result = L1 (1 :: Int) ::: lem (((-1) /\ (-2)) : Nil)
      result `shouldEqual` Sek (L1 1) (Pair (L1 (-1)) (L1 (-2))) Nil

  describe "Operator :+ (postpend)" do
    it "L1 1 :+ 2 -> Sek (L1 1) (L1 2) Nil" do
      let result = L1 (1 :: Int) :+ 2
      result `shouldEqual` Sek (L1 1) (L1 2) Nil

    it "(Sek (L1 1) (L1 2) Nil) :+ 3 -> extends Sek" do
      let result = (1 +: L1 (2 :: Int)) :+ 3
      result `shouldEqual` Sek (L1 1) (L1 2) (L1 3 : Nil) 

    it "L1 1 :+ (2 /\\ 3) -> creates Sekdict" do
      let result = L1 (1 :: Int) :+ (2 /\ 3)
      result `shouldEqual` Sekdict (S1 (L1 1)) (D1 (L1 2) (L1 3))

    it "Gap :+ 1 -> Sek Gap (L1 1) Nil" do
      let result = Gap :+ 1
      result `shouldEqual` Sek Gap (L1 1) Nil

    it "Choice (L1 1) (L1 2) Nil :+ 3 -> Sek with Choice" do
      let result = (L1 (1 :: Int) \/ L1 (2 :: Int)) :+ 3
      result `shouldEqual` Sek (L1 3) (Choice (L1 1) (L1 2) Nil) Nil
    it "choice of primitives" do
      (1 \/ 2 \/ 3) `shouldEqual` Choice (L1 1) (L1 2) (L1 3 : Nil)

    it "Pair (L1 a) (L1 b) :+ 1 -> Sek with Pair" do
      let result = Pair (L1 (-1)) (L1 (-2)) :+ 1
      result `shouldEqual` Sek (L1 1) (Pair (L1 (-1)) (L1 (-2))) Nil

    it "L1 1 :+ L1 2 -> Sek (L1 2) (L1 1) Nil" do
      let result = L1 (1 :: Int) :+ L1 (2 :: Int)
      result `shouldEqual` Sek (L1 2) (L1 1) Nil

  describe "Operator <+ (addPrimitive)" do
    it "2 <+ L1 1 -> Bag (L1 2) (L1 1) Nil" do
      let result = 2 <+ L1 (1 :: Int)
      result `shouldEqual` Bag (L1 2) (L1 1) Nil

    it "Sek (L1 1) (L1 2) Nil <+ 3 -> extends Sek" do
      let result = 3 <+ (1 +: L1 (2 :: Int))
      result `shouldEqual` Bag (L1 3) (Sek (L1 1) (L1 2) Nil) Nil

    it "Pair (L1 a) (L1 b) <+ (c /\\ d) -> creates Dict" do
      let result = ((-3) /\ (-4)) <+ lem (((-1) /\ (-2)) : Nil)
      result `shouldEqual` Dict (Tuple (L1 (-1)) (L1 (-2))) (Tuple (L1 (-3)) (L1 (-4))) Nil

  describe "Operator <+> (combine)" do
    it "L1 1 <+> L1 2 -> Bag (L1 1) (L1 2) Nil" do
      let result = L1 (1 :: Int) <+> L1 (2 :: Int)
      result `shouldEqual` Bag (L1 1) (L1 2) Nil

    it "Bag (L1 1) (L1 2) Nil <+> Bag (L1 3) (L1 4) Nil -> combines into larger Bag" do
      let result = (L1 (1 :: Int) <+> L1 (2 :: Int)) <+> (L1 (3 :: Int) <+> L1 (4 :: Int))
      result `shouldEqual` Bag (L1 1) (L1 2) (L1 3 : L1 4 : Nil)

    it "Sek (L1 1) (L1 2) Nil <+> L1 3 -> creates Bag" do
      let result = (1 +: L1 (2 :: Int)) <+> L1 (3 :: Int)
      result `shouldEqual` Bag (Sek (L1 1) (L1 2) Nil) (L1 3) Nil

    it "Pair (L1 a) (L1 b) <+> L1 1 -> creates Bagdict" do
      let result = lem (((-1) /\ (-2)) : Nil) <+> L1 (1 :: Int)
      result `shouldEqual` Bagdict (B1 (L1 1)) (D1 (L1 (-1)) (L1 (-2)))

  describe "Operator +: with Pair and Dict" do
    it "1 +: Pair (L1 a) (L1 b) -> Sek with L1 and Pair" do
      let result = 1 +: lem (((-1) /\ (-2)) : Nil)
      result `shouldEqual` Sek (L1 1) (Pair (L1 (-1)) (L1 (-2))) Nil

    it "1 +: Dict (Tuple (L1 a) (L1 b)) (Tuple (L1 c) (L1 d)) Nil -> Sek with L1 and Dict" do
      let result = 1 +: lem (((-1) /\ (-2)) : ((-3) /\ (-4)) : Nil)
      result `shouldEqual` Sek (L1 1) (Dict (Tuple (L1 (-1)) (L1 (-2))) (Tuple (L1 (-3)) (L1 (-4))) Nil) Nil

    it "(1 /\\ 2) +: Pair (L1 a) (L1 b) -> Sek with Pairs" do
      let result = (1 /\ 2) +: lem (((-1) /\ (-2)) : Nil)
      result `shouldEqual` Sek (Pair (L1 1) (L1 2)) (Pair (L1 (-1)) (L1 (-2))) Nil

    it "(1 /\\ 2) +: Dict (Tuple (L1 a) (L1 b)) (Tuple (L1 c) (L1 d)) Nil -> Sek with Pair and Dict" do
      let result = (1 /\ 2) +: lem (((-1) /\ (-2)) : ((-3) /\ (-4)) : Nil)
      result `shouldEqual` Sek (Pair (L1 1) (L1 2)) (Dict (Tuple (L1 (-1)) (L1 (-2))) (Tuple (L1 (-3)) (L1 (-4))) Nil) Nil

  describe "Chaining operators" do
    it "1 +: (2 +: L1 3) -> flat Sek" do
      let result = 1 +: (2 +: L1 (3 :: Int))
      result `shouldEqual` Sek (L1 1) (L1 2) (L1 3 : Nil)

    it "L1 1 ::: L1 2 ::: L1 3 -> left-associative nesting" do
      let result = L1 (1 :: Int) ::: L1 (2 :: Int) ::: L1 (3 :: Int)
      result `shouldEqual` Sek (Sek (L1 1) (L1 2) Nil) (L1 3) Nil

    it "L1 1 <+> L1 2 <+> L1 3 -> flat Bag" do
      let result = L1 (1 :: Int) <+> L1 (2 :: Int) <+> L1 (3 :: Int)
      result `shouldEqual` Bag (L1 1) (L1 2) ((L1 3) : Nil)

  describe "Mixed operator combinations" do
    it "(1 +: L1 2) ::: (3 +: L1 4) -> nested Sek of Seks" do
      let result = (1 +: L1 (2 :: Int)) ::: (3 +: L1 (4 :: Int))
      result `shouldEqual` Sek (Sek (L1 1) (L1 2) Nil) (Sek (L1 3) (L1 4) Nil) Nil

    it "(L1 1 <+> L1 2) <+ 3 -> adds to existing Bag" do
      let result = 3 <+ (L1 (1 :: Int) <+> L1 (2 :: Int))
      result `shouldEqual` Bag (L1 1) (L1 2) (L1 3 : Nil)

  describe "Operator \\/ (or/choice)" do
    it "L1 1 \\/ L1 2 -> Choice (L1 1) (L1 2) Nil" do
      let result = L1 (1 :: Int) \/ L1 (2 :: Int)
      result `shouldEqual` Choice (L1 1) (L1 2) Nil

    it "1 \\/ L1 2 -> Choice (L1 1) (L1 2) Nil" do
      let result = (1 :: Int) \/ L1 (2 :: Int)
      result `shouldEqual` Choice (L1 1) (L1 2) Nil

    it "L1 1 \\/ L0 -> L1 1 (special case)" do
      let result = L1 (1 :: Int) \/ (L0 :: Lem Int)
      result `shouldEqual` L1 1

    it "Sek (L1 1) (L1 2) Nil \\/ L1 3 -> Choice with Sek and L1" do
      let result = ((1 :: Int) +: L1 (2 :: Int)) \/ L1 (3 :: Int)
      result `shouldEqual` Choice (Sek (L1 1) (L1 2) Nil) (L1 3) Nil

    it "removes duplicates: L1 1 \\/ L1 1 -> L1 1" do
      let result = L1 (1 :: Int) \/ L1 (1 :: Int)
      result `shouldEqual` L1 1

    it "removes duplicates: 1 \\/ L1 1 -> L1 1" do
      let result = (1 :: Int) \/ L1 (1 :: Int)
      result `shouldEqual` L1 1

    it "Choice (L1 1) (L1 2) Nil \\/ L1 3 -> flattens to single Choice" do
      let result = (L1 (1 :: Int) \/ L1 (2 :: Int)) \/ L1 (3 :: Int)
      result `shouldEqual` Choice (L1 1) (L1 2) ((L1 3) : Nil)

    it "removes duplicates: Choice (L1 1) (L1 2) Nil \\/ L1 1 -> collapses to original" do
      let result = (L1 (1 :: Int) \/ L1 (2 :: Int)) \/ L1 (1 :: Int)
      result `shouldEqual` Choice (L1 1) (L1 2) Nil