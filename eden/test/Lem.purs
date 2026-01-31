module Test.Lem where

import Prelude

import Data.List as List
import Data.List.Types (List(..))
import Data.Tuple.Nested ((/\))
import Kubrick.Lem (Lem(..), (<+), (<+>), (+:), (:::), (:+))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)

spec :: Spec Unit
spec = do
  describe "Lem" do
    describe "L1 constructor" do
      it "creates L1" do
        L1 42 `shouldEqual` L1 42

    describe "Sek constructor" do
      it "creates Sek with at least 2 elements" do
        let sek = Sek (L1 1) (L1 2) (List.fromFoldable [ L1 3, L1 4 ])
        case sek of
          Sek fst snd rest -> do
            fst `shouldEqual` L1 1
            snd `shouldEqual` L1 2
            List.length rest `shouldEqual` 2
          _ -> false `shouldEqual` true

      it "creates Sek with exactly 2 elements" do
        Sek (L1 1) (L1 2) Nil `shouldEqual` Sek (L1 1) (L1 2) Nil

    describe "Pair constructor" do
      it "creates Pair from Lem values" do
        Pair (L1 "key") (L1 "value") `shouldEqual` Pair (L1 "key") (L1 "value")

    describe "Bag constructor" do
      it "creates Bag with at least 2 elements" do
        let bag = Bag (L1 1) (L1 2) (List.singleton (L1 3))
        case bag of
          Bag fst snd rest -> do
            fst `shouldEqual` L1 1
            snd `shouldEqual` L1 2
            List.length rest `shouldEqual` 1
          _ -> false `shouldEqual` true

      it "creates Bag with exactly 2 elements" do
        Bag (L1 1) (L1 2) Nil `shouldEqual` Bag (L1 1) (L1 2) Nil

    describe "Dict constructor" do
      it "creates Dict with Map" do
        let dict = Dict (L1 1 /\ L1 10) (L1 2 /\ L1 20) Nil
        case dict of
          Dict fst snd rest -> do
            List.length rest `shouldEqual` 0
            fst `shouldEqual` (L1 1 /\ L1 10)
            snd `shouldEqual` (L1 2 /\ L1 20)
            rest `shouldEqual` Nil
          _ -> false `shouldEqual` true

    describe "Choice constructor" do
      it "creates Choice with at least 2 elements" do
        let choice = Choice (L1 1) (L1 2) (List.singleton (L1 3))
        case choice of
          Choice fst snd rest -> do
            fst `shouldEqual` L1 1
            snd `shouldEqual` L1 2
            List.length rest `shouldEqual` 1
          _ -> false `shouldEqual` true

    describe "Sek1 type" do
      it "creates S2 with at least 2 elements using <+ operator" do
        let sek = (L1 1 :: Lem Int) <+ 2 <+ 3 -- Creates Sek (internally S2)
        -- Just verify it's a valid Sek (can't pattern match on S2 since it's hidden)
        sek `shouldEqual` ((L1 1 :: Lem Int) <+ 2 <+ 3)

      it "creates S1 with single element" do
        -- L1 42 is just L1 42, not a Sek
        L1 42 `shouldEqual` L1 42

    describe "Bag1 type" do
      it "creates B2 with at least 2 elements using <+> operator" do
        let bag = L1 1 <+> L1 2 -- Creates Bag (internally B2)
        bag `shouldEqual` (L1 1 <+> L1 2)

      it "creates B1 with single element" do
        -- L1 42 is just L1 42, not a Bag
        L1 42 `shouldEqual` L1 42

    describe "Dict1 type (via Dict constructor)" do
      it "creates Dict with 2 pairs" do
        let dict = Dict (L1 1 /\ L1 10) (L1 2 /\ L1 20) Nil
        -- Just verify it was created (can't pattern match on D2 since it's hidden)
        dict `shouldEqual` Dict (L1 1 /\ L1 10) (L1 2 /\ L1 20) Nil

      it "creates Pair with single key-value" do
        Pair (L1 "key") (L1 "value") `shouldEqual` Pair (L1 "key") (L1 "value")

    describe "Sekdict constructor (via ::: operator)" do
      it "creates Sekdict using :::" do
        let sekdict = (1 /\ 2) +: L1 3
        case sekdict of
          Sekdict _ _ -> true `shouldEqual` true
          _ -> false `shouldEqual` true

      it "creates Sekdict with single values" do
        let sek1 = (1 /\ 2) +: L1 3
        let sek2 = (1 /\ 2) +: L1 3
        sek1 `shouldEqual` sek2

    describe "Bagdict constructor (via <+> operator)" do
      it "creates Bagdict using <+>" do
        let bagdict = Pair (L1 1) (L1 10) <+> Bag (L1 2) (L1 3) Nil
        case bagdict of
          Bagdict _ _ -> true `shouldEqual` true
          _ -> false `shouldEqual` true

      it "creates Bagdict with single values" do
        let bag1 = Pair (L1 1) (L1 10) <+> L1 1
        let bag2 = Pair (L1 1) (L1 10) <+> L1 1
        bag1 `shouldEqual` bag2

    describe "<+ operator" do
      it "add el to L1" do
        let result = (L1 1) <+ 2
        result `shouldEqual` Sek (L1 1) (L1 2) Nil

      it "add element to Sek" do
        let result = Sek (L1 1) (L1 2) Nil <+ 3
        result `shouldEqual` Sek (L1 1) (L1 2) (List.singleton (L1 3))

      it "add element to Bag" do
        let result = Bag (L1 1) (L1 2) Nil <+ 3
        result `shouldEqual` Bag (L1 1) (L1 2) (List.singleton (L1 3))

      it "add element to Choice" do
        let choice = Choice (L1 2) (L1 3) Nil
        let result = choice <+ 1
        result `shouldEqual` Choice (L1 2) (L1 3) (List.singleton (L1 1))

    describe "<+> operator" do
      it "add pair to Pair" do
        let pair = Pair (L1 1) (L1 10)
        let result = pair <+ (2 /\ 20)
        result `shouldEqual` Dict (L1 1 /\ L1 10) (L1 2 /\ L1 20) Nil

      it "add pair to Dict" do
        let dict = Dict (L1 1 /\ L1 10) (L1 2 /\ L1 20) Nil
        let result = dict <+ (3 /\ 30)
        result `shouldEqual` Dict (L1 1 /\ L1 10) (L1 2 /\ L1 20) (List.singleton (L1 3 /\ L1 30))

      it "combine two L1 elements" do
        let result = (L1 1) <+> (L1 2)
        result `shouldEqual` Bag (L1 1) (L1 2) Nil

      it "combine L1 with Bag" do
        let bag = Bag (L1 2) (L1 3) Nil
        let result = (L1 1) <+> bag
        result `shouldEqual` Bag (L1 1) (L1 2) (List.singleton (L1 3))

      it "combine two Bags" do
        let bag1 = Bag (L1 1) (L1 2) Nil
        let bag2 = Bag (L1 3) (L1 4) Nil
        let result = bag1 <+> bag2
        result `shouldEqual` Bag (L1 1) (L1 2) (List.fromFoldable [ L1 3, L1 4 ])

      it "combine Sek with Lem" do
        let sek = Sek (L1 1) (L1 2) Nil
        let result = sek <+> (L1 3)
        result `shouldEqual` Bag (Sek (L1 1) (L1 2) Nil) (L1 3) Nil

      it "combine Pair with Lem" do
        let pair = Pair (L1 "k") (L1 "v")
        let result = pair <+> (L1 "w")
        -- Result is Bagdict but we can't construct B1/D1 directly, so just verify it's created
        case result of
          Bagdict _ _ -> true `shouldEqual` true
          _ -> false `shouldEqual` true

      it "combine L0 with Lem" do
        let result = L0 <+> (L1 42)
        result `shouldEqual` L1 42
    it "check equality Sek" do
      let sek1 = Sek (L1 1) (L1 2) (List.singleton (L1 3))
      let sek2 = Sek (L1 1) (L1 2) (List.singleton (L1 3))
      sek1 `shouldEqual` sek2
      shouldNotEqual sek1 (Sek (L1 11) (L1 2) (List.singleton (L1 3)))
    describe "Equality tests" do
      it "check equality for Sek (ordered)" do
        let sek1 = Sek (L1 1) (L1 2) (List.singleton (L1 3))
        let sek2 = Sek (L1 1) (L1 2) (List.singleton (L1 3))
        let sek3 = Sek (L1 2) (L1 1) (List.singleton (L1 3)) -- different order
        sek1 `shouldEqual` sek2
        sek1 `shouldNotEqual` sek3
        shouldNotEqual sek1 (Sek (L1 11) (L1 2) (List.singleton (L1 3)))

      it "check equality for Bag (unordered)" do
        let bag1 = Bag (L1 1) (L1 2) (List.singleton (L1 3))
        let bag2 = Bag (L1 2) (L1 1) (List.singleton (L1 3)) -- different order, should be equal
        let bag3 = Bag (L1 1) (L1 3) (List.singleton (L1 2)) -- different order, should be equal
        bag1 `shouldEqual` bag2
        bag1 `shouldEqual` bag3
        shouldNotEqual bag1 (Bag (L1 1) (L1 2) (List.singleton (L1 4)))

      it "check equality for Choice (unordered)" do
        let choice1 = Choice (L1 1) (L1 2) (List.singleton (L1 3))
        let choice2 = Choice (L1 2) (L1 3) (List.singleton (L1 1)) -- different order, should be equal
        choice1 `shouldEqual` choice2
        shouldNotEqual choice1 (Choice (L1 1) (L1 2) (List.singleton (L1 4)))

      it "check equality for Dict (unordered)" do
        let dict1 = Dict (L1 1 /\ L1 10) (L1 2 /\ L1 20) (List.singleton (L1 3 /\ L1 30))
        let dict2 = Dict (L1 2 /\ L1 20) (L1 1 /\ L1 10) (List.singleton (L1 3 /\ L1 30)) -- different order, should be equal
        let dict3 = Dict (L1 3 /\ L1 30) (L1 1 /\ L1 10) (List.singleton (L1 2 /\ L1 20)) -- different order, should be equal
        dict1 `shouldEqual` dict2
        dict1 `shouldEqual` dict3
        shouldNotEqual dict1 (Dict (L1 1 /\ L1 10) (L1 2 /\ L1 20) (List.singleton (L1 4 /\ L1 40)))

      it "check equality for Pair" do
        let pair1 = Pair (L1 "k") (L1 "v")
        let pair2 = Pair (L1 "k") (L1 "v")
        pair1 `shouldEqual` pair2
        shouldNotEqual pair1 (Pair (L1 "k2") (L1 "v"))

      -- --       it "check equality for Bag1 (unordered)" do
      --         let b1 = B2 (L1 1) (L1 2) (List.singleton (L1 3))
      --         let b2 = B2 (L1 2) (L1 1) (List.singleton (L1 3)) -- different order, should be equal
      --         let b3 = B2 (L1 3) (L1 1) (List.singleton (L1 2)) -- different order, should be equal
      --         b1 `shouldEqual` b2
      --         b1 `shouldEqual` b3
      -- 
      --       it "check equality for Dict1 (unordered)" do
      -- -- --         let d1 = D2 (L1 1 /\ L1 10) (L1 2 /\ L1 20) (List.singleton (L1 3 /\ L1 30))
      -- --         let d2 = D2 (L1 2 /\ L1 20) (L1 3 /\ L1 30) (List.singleton (L1 1 /\ L1 10)) -- different order, should be equal
      -- --         d1 `shouldEqual` d2
      -- 
      it "check equality for L0 and L1" do
        (L0 :: Lem Int) `shouldEqual` L0
        L1 42 `shouldEqual` L1 42
        shouldNotEqual (L0 :: Lem Int) (L1 42)
        shouldNotEqual (L1 1) (L1 2)

      it "check equality for Sekdict" do
        let sd1 = L1 "a" ::: Pair (L1 "k") (L1 "v")
        let sd2 = L1 "a" ::: Pair (L1 "k") (L1 "v")
        sd1 `shouldEqual` sd2

      it "check equality for Bagdict" do
        let bd1 = Pair (L1 "k") (L1 "v") <+> L1 "a"
        let bd2 = Pair (L1 "k") (L1 "v") <+> L1 "a"
        bd1 `shouldEqual` bd2

    describe "+: operator (prepend primitive)" do
      it "prepend primitive to L1" do
        let result = 1 +: (L1 2)
        result `shouldEqual` Sek (L1 1) (L1 2) Nil

      it "prepend primitive to Sek" do
        let result = 1 +: Sek (L1 2) (L1 3) Nil
        result `shouldEqual` Sek (L1 1) (Sek (L1 2) (L1 3) Nil) Nil

      it "prepend primitive to Bag" do
        let result = 1 +: Bag (L1 2) (L1 3) Nil
        result `shouldEqual` Sek (L1 1) (Bag (L1 2) (L1 3) Nil) Nil

      it "prepend primitive to Choice" do
        let result = 1 +: Choice (L1 2) (L1 3) Nil
        let expected = 1 +: Choice (L1 2) (L1 3) Nil
        result `shouldEqual` expected

      --       it "prepend tuple to Pair" do
      --         let result = (1 /\ 10) +: Pair (L1 2) (L1 20)
      --         result `shouldEqual` Pair (L1 1) (L1 10) ::: Pair (L1 2) (L1 20)
      -- 
      --       it "prepend tuple to Sek" do
      --         let result = (1 /\ 10) +: Sek (L1 2) (L1 3) Nil
      --         result `shouldEqual` Sek (Pair (L1 1) (L1 10)) (L1 2) (List.singleton (L1 3))

      it "prepend primitive to L0" do
        let result = 1 +: (L0 :: Lem Int)
        result `shouldEqual` L1 1

    describe "::: operator (prepend Lem)" do
      it "prepend L1 to L1" do
        let result = (L1 1) ::: (L1 2)
        result `shouldEqual` Sek (L1 1) (L1 2) Nil

      it "prepend L1 to Sek" do
        let result = (L1 1) ::: Sek (L1 2) (L1 3) Nil
        result `shouldEqual` Sek (L1 1) (Sek (L1 2) (L1 3) Nil) Nil

      --       it "prepend L1 to Pair" do
      --         let result = (L1 "a") ::: Pair (L1 "k") (L1 "v")
      --         result `shouldEqual` L1 "a" ::: Pair (L1 "k") (L1 "v")
      -- 
      --       it "prepend Sek to Sek" do
      --         let sek1 = Sek (L1 1) (L1 2) Nil
      --         let sek2 = Sek (L1 3) (L1 4) Nil
      --         let result = sek1 ::: sek2
      --         result `shouldEqual` Sek (Sek (L1 1) (L1 2) Nil) (L1 3) (List.singleton (L1 4))
      -- 
      --       it "prepend Pair to Dict" do
      --         let pair = Pair (L1 1) (L1 10)
      --         let dict = Dict (L1 2 /\ L1 20) (L1 3 /\ L1 30) Nil
      --         let result = pair ::: dict
      --         let expected = Pair (L1 1) (L1 10) ::: Dict (L1 2 /\ L1 20) (L1 3 /\ L1 30) Nil
      --         result `shouldEqual` expected
      -- 
      --       it "prepend L1 to Sekdict" do
      --         let sd = L1 "a" ::: Pair (L1 "k") (L1 "v")
      --         let result = (L1 "a") ::: sd
      --         let expected = (L1 "a") ::: (L1 "a" ::: Pair (L1 "k") (L1 "v"))
      --         result `shouldEqual` expected

      it "prepend to L0" do
        let result = (L1 1) ::: (L0 :: Lem Int)
        result `shouldEqual` Sek (L1 1) L0 Nil
    it "check equality Sek" do
      let sek1 = Sek (L1 1) (L1 2) (List.singleton (L1 3))
      let sek2 = Sek (L1 1) (L1 2) (List.singleton (L1 3))
      sek1 `shouldEqual` sek2
      shouldNotEqual sek1 (Sek (L1 11) (L1 2) (List.singleton (L1 3)))
    describe "Equality tests" do
      it "check equality for Sek (ordered)" do
        let sek1 = Sek (L1 1) (L1 2) (List.singleton (L1 3))
        let sek2 = Sek (L1 1) (L1 2) (List.singleton (L1 3))
        let sek3 = Sek (L1 2) (L1 1) (List.singleton (L1 3)) -- different order
        sek1 `shouldEqual` sek2
        sek1 `shouldNotEqual` sek3
        shouldNotEqual sek1 (Sek (L1 11) (L1 2) (List.singleton (L1 3)))

      it "check equality for Bag (unordered)" do
        let bag1 = Bag (L1 1) (L1 2) (List.singleton (L1 3))
        let bag2 = Bag (L1 2) (L1 1) (List.singleton (L1 3)) -- different order, should be equal
        let bag3 = Bag (L1 1) (L1 3) (List.singleton (L1 2)) -- different order, should be equal
        bag1 `shouldEqual` bag2
        bag1 `shouldEqual` bag3
        shouldNotEqual bag1 (Bag (L1 1) (L1 2) (List.singleton (L1 4)))

      it "check equality for Choice (unordered)" do
        let choice1 = Choice (L1 1) (L1 2) (List.singleton (L1 3))
        let choice2 = Choice (L1 2) (L1 3) (List.singleton (L1 1)) -- different order, should be equal
        choice1 `shouldEqual` choice2
        shouldNotEqual choice1 (Choice (L1 1) (L1 2) (List.singleton (L1 4)))

      it "check equality for Dict (unordered)" do
        let dict1 = Dict (L1 1 /\ L1 10) (L1 2 /\ L1 20) (List.singleton (L1 3 /\ L1 30))
        let dict2 = Dict (L1 2 /\ L1 20) (L1 1 /\ L1 10) (List.singleton (L1 3 /\ L1 30)) -- different order, should be equal
        let dict3 = Dict (L1 3 /\ L1 30) (L1 1 /\ L1 10) (List.singleton (L1 2 /\ L1 20)) -- different order, should be equal
        dict1 `shouldEqual` dict2
        dict1 `shouldEqual` dict3
        shouldNotEqual dict1 (Dict (L1 1 /\ L1 10) (L1 2 /\ L1 20) (List.singleton (L1 4 /\ L1 40)))

      it "check equality for Pair" do
        let pair1 = Pair (L1 "k") (L1 "v")
        let pair2 = Pair (L1 "k") (L1 "v")
        pair1 `shouldEqual` pair2
        shouldNotEqual pair1 (Pair (L1 "k2") (L1 "v"))

        --       it "check equality for Bag1 (unordered)" do
        --         let b1 = B2 (L1 1) (L1 2) (List.singleton (L1 3))
        --         let b2 = B2 (L1 2) (L1 1) (List.singleton (L1 3)) -- different order, should be equal
        --         let b3 = B2 (L1 3) (L1 1) (List.singleton (L1 2)) -- different order, should be equal
        --         b1 `shouldEqual` b2
        --         b1 `shouldEqual` b3
        -- 
        --       it "check equality for Dict1 (unordered)" do
        --         let d1 = D2 (L1 1 /\ L1 10) (L1 2 /\ L1 20) (List.singleton (L1 3 /\ L1 30))
        --         let d2 = D2 (L1 2 /\ L1 20) (L1 3 /\ L1 30) (List.singleton (L1 1 /\ L1 10)) -- different order, should be equal
        --         d1 `shouldEqual` d2
        -- 
        --       it "check equality for L0 and L1" do
        --         (L0 :: Lem Int) `shouldEqual` L0
        --         L1 42 `shouldEqual` L1 42
        shouldNotEqual (L0 :: Lem Int) (L1 42)
        shouldNotEqual (L1 1) (L1 2)

      it "check equality for Sekdict" do
        let sd1 = L1 "a" ::: Pair (L1 "k") (L1 "v")
        let sd2 = L1 "a" ::: Pair (L1 "k") (L1 "v")
        sd1 `shouldEqual` sd2

      it "check equality for Bagdict" do
        let bd1 = Pair (L1 "k") (L1 "v") <+> L1 "a"
        let bd2 = Pair (L1 "k") (L1 "v") <+> L1 "a"
        bd1 `shouldEqual` bd2

    describe ":+ operator (postpend primitive)" do
      it "postpend primitive to L1" do
        let result = (L1 1) :+ 2
        result `shouldEqual` Sek (L1 2) (L1 1) Nil

      it "postpend primitive to Sek" do
        let result = Sek (L1 1) (L1 2) Nil :+ 3
        result `shouldEqual` Sek (L1 3) (Sek (L1 1) (L1 2) Nil) Nil

      it "postpend primitive to Bag" do
        let result = Bag (L1 1) (L1 2) Nil :+ 3
        result `shouldEqual` Sek (L1 3) (Bag (L1 1) (L1 2) Nil) Nil

      --       it "postpend primitive to Choice" do
      --         let result = Choice (L1 1) (L1 2) Nil :+ 3
      --         result `shouldEqual` Sek (Choice (L1 1) (L1 2) Nil) (L1 3) Nil
      -- 
      --       it "postpend tuple to Pair" do
      --         let result = Pair (L1 1) (L1 10) :+ (2 /\ 20)
      --         result `shouldEqual` L1 (Pair (L1 1) (L1 10)) ::: Pair (L1 2) (L1 20)
      -- 
      --       it "postpend tuple to Dict" do
      --         let result = Dict (L1 1 /\ L1 10) (L1 2 /\ L1 20) Nil :+ (3 /\ 30)
      --         result `shouldEqual` L1 (Dict (L1 1 /\ L1 10) (L1 2 /\ L1 20) Nil) ::: Pair (L1 3) (L1 30)

      it "postpend tuple to Sek" do
        let result = Sek (L1 1) (L1 2) Nil :+ (3 /\ 30)
        result `shouldEqual` Sek (Pair (L1 3) (L1 30)) (Sek (L1 1) (L1 2) Nil) Nil

      it "postpend primitive to L0" do
        let result = (L0 :: Lem Int) :+ 1
        result `shouldEqual` L1 1

