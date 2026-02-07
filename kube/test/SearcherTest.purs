module Test.Kube.SearcherTest where

import Prelude

import Data.Array as Array
import Data.Either (Either(..), isLeft, isRight)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.List ((:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Kubrick.Kube (Kid(..), add)
import Kubrick.Kube.Types (emptyKube)
import Kubrick.Lem (Lem(..), (+:), (<+), (<+>), (:::), lem)
import Kubrick.Searcher (fill)
import Kubrick.Types (Raw(..), Vid(..), Term(..))
import Kubrick.Reticolo as Reticolo
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy, fail)

spec :: Spec Unit
spec = describe "Searcher" do
  describe "fill" do
    it "extracts values from simple L1 document" do
      let kube = emptyKube
      let doc = Rs "hello" +: L0
      let query = TVid (Vid 0) +: L0
      let (kube2 /\ kid) = add kube doc
      let result = fill kube2 kid query
      result `shouldSatisfy` isRight
      case result of
        Right ret -> do
          Reticolo.rowCount ret `shouldEqual` 1
          Reticolo.columnCount ret `shouldEqual` 1
        Left _ -> pure unit
    
    it "extracts multiple values from Sek" do
      let kube = emptyKube
      let doc = Rs "a" +: Rs "b" +: Rs "c" +: L0
      let query = TVid (Vid 0) +: TVid (Vid 1) +: TVid (Vid 2) +: L0
      let (kube2 /\ kid) = add kube doc
      let result = fill kube2 kid query
      result `shouldSatisfy` isRight
      case result of
        Right ret -> do
          Reticolo.rowCount ret `shouldEqual` 1
          Reticolo.columnCount ret `shouldEqual` 3
          case Reticolo.getRow 0 ret of
            Just row -> row `shouldEqual` [Rs "a", Rs "b", Rs "c"]
            Nothing -> fail "Row 0 not found"
        Left _ -> pure unit
    
    it "skips Gap values in query" do
      let kube = emptyKube
      let doc = Rs "a" +: Rs "b" +: Rs "c" +: L0
      let query = Sek (TVid (Vid 0) +: L0) Gap ((TVid (Vid 1) +: L0) : List.Nil)
      let (kube2 /\ kid) = add kube doc
      let result = fill kube2 kid query
      result `shouldSatisfy` isRight
      case result of
        Right ret -> do
          Reticolo.rowCount ret `shouldEqual` 1
          Reticolo.columnCount ret `shouldEqual` 2
          case Reticolo.getRow 0 ret of
            Just row -> row `shouldEqual` [Rs "a", Rs "c"]
            Nothing -> fail "Row 0 not found"
        Left _ -> pure unit
    
    it "skips TRaw values in query" do
      let kube = emptyKube
      let doc = Rs "a" +: Rs "b" +: Rs "c" +: L0
      let query = TVid (Vid 0) +: TRaw (Rs "b") +: TVid (Vid 1) +: L0
      let (kube2 /\ kid) = add kube doc
      let result = fill kube2 kid query
      result `shouldSatisfy` isRight
      case result of
        Right ret -> do
          Reticolo.rowCount ret `shouldEqual` 1
          Reticolo.columnCount ret `shouldEqual` 2
          case Reticolo.getRow 0 ret of
            Just row -> row `shouldEqual` [Rs "a", Rs "c"]
            Nothing -> fail "Row 0 not found"
        Left _ -> pure unit

    it "returns error when Kid not found" do
      let kube = emptyKube
      let query = TVid (Vid 0) +: L0
      let result = fill kube (Kid 999) query
      result `shouldSatisfy` isLeft
    
    it "returns error when structure doesn't match" do
      let kube = emptyKube
      let doc = Rs "hello" +: L0
      let query = TVid (Vid 0) +: TVid (Vid 1) +: L0
      let (kube2 /\ kid) = add kube doc
      let result = fill kube2 kid query
      result `shouldSatisfy` isLeft
    
    it "omits Vid pointing to nested structure" do
      let kube = emptyKube
      let nestedSek :: Lem Raw
          nestedSek = Rs "b" +: Rs "c" +: L0
      let doc = (Rs "a" +: L0) ::: nestedSek
      let query = (TVid (Vid 0) +: L0) ::: (TVid (Vid 1) +: L0)
      let (kube2 /\ kid) = add kube doc
      let result = fill kube2 kid query
      -- Should succeed but omit the nested part
      case result of
        Right ret -> do
          Reticolo.rowCount ret `shouldEqual` 1
          -- Only Vid 0 has a value, Vid 1 points to nested structure so is omitted
          Reticolo.columnCount ret `shouldEqual` 1
          case Reticolo.getRow 0 ret of
            Just row -> do
              Array.length row `shouldEqual` 1
              row `shouldEqual` [Rs "a"]
            Nothing -> fail "Row 0 not found"
        Left err -> fail $ "Expected success but got: " <> err

    it "generates cross-product for Bag with multiple Vids" do
      let kube = emptyKube
      let doc = Rs "a" <+ Rs "b" <+ L0
      let query = TVid (Vid 0) <+ TVid (Vid 1) <+ L0
      let (kube2 /\ kid) = add kube doc
      let result = fill kube2 kid query
      case result of
        Right ret -> do
          Reticolo.rowCount ret `shouldEqual` 2
          Reticolo.columnCount ret `shouldEqual` 2
          -- Row 0: Vid 0 -> "a", Vid 1 -> "b"
          -- Row 1: Vid 0 -> "b", Vid 1 -> "a"
          case Reticolo.getRow 0 ret of
            Just row -> row `shouldEqual` [Rs "a", Rs "b"]
            Nothing -> fail "Row 0 not found"
          case Reticolo.getRow 1 ret of
            Just row -> row `shouldEqual` [Rs "b", Rs "a"]
            Nothing -> fail "Row 1 not found"
        Left err -> fail $ "Expected success but got: " <> err

    it "generates single row for Bag with mixed Vid and TRaw" do
      let kube = emptyKube
      let doc = (Rs "a" +: L0) <+> (Rs "x" +: L0)
      let query = (TVid (Vid 0) +: L0) <+> (TRaw (Rs "x") +: L0)
      let (kube2 /\ kid) = add kube doc
      let result = fill kube2 kid query
      case result of
        Right ret -> do
          Reticolo.rowCount ret `shouldEqual` 1
          Reticolo.columnCount ret `shouldEqual` 1
          -- Only "a" should be extracted (at Vid 0 position)
          case Reticolo.getRow 0 ret of
            Just row -> row `shouldEqual` [Rs "a"]
            Nothing -> fail "Row 0 not found"
        Left err -> fail $ "Expected success but got: " <> err

    it "extracts values from Pair" do
      let kube = emptyKube
      let doc = Pair (L1 (Rs "key")) (L1 (Rs "value"))
      let query = Pair (TVid (Vid 0) +: L0) (TVid (Vid 1) +: L0)
      let (kube2 /\ kid) = add kube doc
      let result = fill kube2 kid query
      case result of
        Right ret -> do
          Reticolo.rowCount ret `shouldEqual` 1
          Reticolo.columnCount ret `shouldEqual` 2
          case Reticolo.getRow 0 ret of
            Just row -> row `shouldEqual` [Rs "key", Rs "value"]
            Nothing -> fail "Row 0 not found"
        Left err -> fail $ "Expected success but got: " <> err

    it "extracts values from Dict with single pair" do
      let kube = emptyKube
      let doc = lem (((Rs "k1") /\ (Rs "v1")) : List.Nil)
      let query = lem (((TVid (Vid 0)) /\ (TVid (Vid 1))) : List.Nil)
      let (kube2 /\ kid) = add kube doc
      let result = fill kube2 kid query
      case result of
        Right ret -> do
          Reticolo.rowCount ret `shouldEqual` 1
          Reticolo.columnCount ret `shouldEqual` 2
          case Reticolo.getRow 0 ret of
            Just row -> row `shouldEqual` [Rs "k1", Rs "v1"]
            Nothing -> fail "Row 0 not found"
        Left err -> fail $ "Expected success but got: " <> err

    it "extracts values from Dict with multiple pairs - simple" do
      let kube = emptyKube
      let doc :: Lem Raw
          doc = Dict (Tuple (L1 (Rs "k1")) (L1 (Rs "v1"))) (Tuple (L1 (Rs "k2")) (L1 (Rs "v2"))) (List.Cons (Tuple (L1 (Rs "k3")) (L1 (Rs "v3"))) List.Nil)
      let query :: Lem Term
          query = Dict (Tuple (L1 (TVid (Vid 0))) (L1 (TVid (Vid 1)))) (Tuple (L1 (TVid (Vid 2))) (L1 (TVid (Vid 3)))) (List.Cons (Tuple (L1 (TVid (Vid 4))) (L1 (TVid (Vid 5)))) List.Nil)
      let (kube2 /\ kid) = add kube doc
      let result = fill kube2 kid query
      case result of
        Right ret -> do
          Reticolo.rowCount ret `shouldEqual` 1
          Reticolo.columnCount ret `shouldEqual` 6
        Left err -> fail $ "Expected success but got: " <> err

    it "extracts values from Dict with multiple pairs" do
      let kube = emptyKube
      let doc :: Lem Raw
          doc = Dict (Tuple (L1 (Rs "k1")) (L1 (Rs "v1"))) (Tuple (L1 (Rs "k2")) (L1 (Rs "v2"))) List.Nil
      let query :: Lem Term
          query = Dict (Tuple (L1 (TVid (Vid 0))) (L1 (TVid (Vid 1)))) (Tuple (L1 (TVid (Vid 2))) (L1 (TVid (Vid 3)))) List.Nil
      let (kube2 /\ kid) = add kube doc
      let result = fill kube2 kid query
      case result of
        Right ret -> do
          Reticolo.rowCount ret `shouldEqual` 1
          Reticolo.columnCount ret `shouldEqual` 4
          case Reticolo.getRow 0 ret of
            Just row -> row `shouldEqual` [Rs "k1", Rs "v1", Rs "k2", Rs "v2"]
            Nothing -> fail "Row 0 not found"
        Left err -> fail $ "Expected success but got: " <> err

    it "extracts partial values from Dict with TRaw and Gap" do
      let kube = emptyKube
      let doc :: Lem Raw
          doc = Dict (Tuple (L1 (Rs "k1")) (L1 (Rs "v1"))) (Tuple (L1 (Rs "k2")) (L1 (Rs "v2"))) List.Nil
      let query :: Lem Term
          query = Dict (Tuple (L1 (TVid (Vid 0))) Gap) (Tuple (L1 (TRaw (Rs "k2"))) (L1 (TVid (Vid 1)))) List.Nil
      let (kube2 /\ kid) = add kube doc
      let result = fill kube2 kid query
      case result of
        Right ret -> do
          Reticolo.rowCount ret `shouldEqual` 1
          Reticolo.columnCount ret `shouldEqual` 2
          case Reticolo.getRow 0 ret of
            Just row -> row `shouldEqual` [Rs "k1", Rs "v2"]
            Nothing -> fail "Row 0 not found"
        Left err -> fail $ "Expected success but got: " <> err
