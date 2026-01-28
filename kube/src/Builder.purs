module Kubrick.Builder (add) where

import Prelude

import Control.Monad.State (State, state)
import Data.Array as Array
import Data.Foldable (foldM)
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Kubrick.Kube.Types (Kid(..), Bi, Kube, bi0, put)
import Kubrick.Lem (Lem(..), Sek1(..), Bag1(..), Dict1(..))

-- | Side enum representing different index types
data Side
  = Key
  | Val
  | Root
  | Sets

derive instance eqSide :: Eq Side
derive instance ordSide :: Ord Side


-- | Add a document to the builder using State monad
-- | Returns the Kid that represents this Lem structure
add :: forall t. Ord t => Eq t => Show t => Lem t -> State (Tuple Kid (Kube t)) Kid
add doc = do
  kid <- nextKid
  doDoc doc kid
  modifyBuilder (addRoot kid)
  pure kid

-- | Add positioned data value
addPos :: forall a. Ord a => Int -> a -> Kid -> Kube a -> Kube a
addPos position value kid kube =
  let
    padded = padSeqs bi0 (position + 1) kube.seqs
    current = fromMaybe bi0 (Array.index padded position)
    updated = put value kid current
  in
    case Array.updateAt position updated padded of
      Just newSeqs -> kube { seqs = newSeqs }
      Nothing -> kube

-- | Add positioned Kid reference
addPosRef :: forall a. Ord a => Int -> Kid -> Kid -> Kube a -> Kube a
addPosRef position kidValue targetKid kube =
  let
    padded = padSeqs bi0 (position + 1) kube.refSeqs
    current = fromMaybe bi0 (Array.index padded position)
    updated = put kidValue targetKid current
  in
    case Array.updateAt position updated padded of
      Just newSeqs -> kube { refSeqs = newSeqs }
      Nothing -> kube

-- | Add to roots set
addRoot :: forall a. Kid -> Kube a -> Kube a
addRoot value builder =
  builder { roots = Set.insert value builder.roots }

-- | Add to sets set
addToSets :: forall a. Kid -> Kube a -> Kube a
addToSets value builder =
  builder { sets = Set.insert value builder.sets }

-- | Get next Kid and increment counter
nextKid :: forall a. State (Tuple Kid (Kube a)) Kid
nextKid = state \(Tuple (Kid n) builder) -> Tuple (Kid n) (Tuple (Kid (n + 1)) builder)

-- | Modify builder
modifyBuilder :: forall a. (Kube a -> Kube a) -> State (Tuple Kid (Kube a)) Unit
modifyBuilder f = state \(Tuple kid builder) -> Tuple unit (Tuple kid (f builder))

-- | Process a document based on its type (State monad version)
doDoc :: forall t. Ord t => Eq t => Show t => Lem t -> Kid -> State (Tuple Kid (Kube t)) Unit
doDoc L0 _ = pure unit
doDoc (L1 value) kid = modifyBuilder (addPos 0 value kid)
doDoc (Choice fst snd rest) kid = doChoice (fst : snd : rest) kid Key
doDoc (Pair k v) kid = doSet (Pair k v) kid Key
doDoc (Sek fst snd rest) kid = doSek (Sek fst snd rest) kid
doDoc (Dict fst snd rest) kid = doMap (fst : snd : rest) kid
doDoc (Bag fst snd rest) kid = do
  doChoice (fst : snd : rest) kid Key
  modifyBuilder (addToSets kid)
doDoc (Sekdict sek dict) kid = do
  doSekFromSek1 sek kid
  doDictFromDict1 dict kid
doDoc (Bagdict bag dict) kid = do
  doBagFromBag1 bag kid
  doDictFromDict1 dict kid

-- | Process Sek1 types (State monad)
doSekFromSek1 :: forall t. Ord t => Eq t => Show t => Sek1 t -> Kid -> State (Tuple Kid (Kube t)) Unit
doSekFromSek1 (S1 lem) kid = doDoc lem kid
doSekFromSek1 (S2 fst snd rest) kid = doSek (Sek fst snd rest) kid

-- | Process Bag1 types (State monad)
doBagFromBag1 :: forall t. Ord t => Eq t => Show t => Bag1 t -> Kid -> State (Tuple Kid (Kube t)) Unit
doBagFromBag1 (B1 lem) kid = doDoc lem kid
doBagFromBag1 (B2 fst snd rest) kid = doChoice (fst : snd : rest) kid Key

-- | Process Dict1 types (State monad)
doDictFromDict1 :: forall t. Ord t => Eq t => Show t => Dict1 t -> Kid -> State (Tuple Kid (Kube t)) Unit
doDictFromDict1 (D1 k v) kid = do
  doSet (Pair k v) kid Key
  modifyBuilder (addToSets kid)
doDictFromDict1 (D2 fst snd rest) kid = doMap (fst : snd : rest) kid

-- | Process a Sek structure (State monad)
doSek :: forall t. Ord t => Eq t => Show t => Lem t -> Kid -> State (Tuple Kid (Kube t)) Unit
doSek (Sek fst snd rest) kid =
  let
    allElems = fst : snd : rest
  in
    doLine allElems kid
doSek lem kid = doDoc lem kid

-- | Process a line (array sequence) (State monad)
doLine :: forall t. Ord t => Eq t => Show t => List (Lem t) -> Kid -> State (Tuple Kid (Kube t)) Unit
doLine line kid =
  let
    indexed :: List (Tuple Int (Lem t))
    indexed = List.zip (List.range 0 (List.length line - 1)) line
  in
    void $ foldM (\_ (Tuple position doc) -> doLineElement doc position kid) unit indexed

-- | Process a single line element (State monad)
doLineElement :: forall t. Ord t => Eq t => Show t => Lem t -> Int -> Kid -> State (Tuple Kid (Kube t)) Unit
doLineElement (Sek fst snd rest) position kid = do
  nid <- nextKid
  doSek (Sek fst snd rest) nid
  modifyBuilder (addPosRef position nid kid)
doLineElement (L1 value) position kid =
  modifyBuilder (addPos position value kid)
doLineElement doc position kid = do
  nid <- nextKid
  doDoc doc nid
  modifyBuilder (addPosRef position nid kid)

-- | Process a map (dictionary) (State monad)
doMap :: forall t. Ord t => Eq t => Show t => List (Tuple (Lem t) (Lem t)) -> Kid -> State (Tuple Kid (Kube t)) Unit
doMap entries parent = do
  void $ foldM (\_ entry -> doMapEntry parent entry) unit entries
  modifyBuilder (addToSets parent)

-- | Process a single map entry (State monad)
doMapEntry :: forall t. Ord t => Eq t => Show t => Kid -> Tuple (Lem t) (Lem t) -> State (Tuple Kid (Kube t)) Unit
doMapEntry parent (Tuple key value) = case key /\ value of
  (Choice fst snd rest) /\ L0 -> doChoice (fst : snd : rest) parent Key
  doc /\ L0 -> doSet doc parent Key
  k /\ v -> doSet (Pair k v) parent Key

-- | Process a choice (alternatives) (State monad)
doChoice :: forall t. Ord t => Eq t => Show t => List (Lem t) -> Kid -> Side -> State (Tuple Kid (Kube t)) Unit
doChoice alts parent side =
  void $ foldM (\_ doc -> doSet doc parent side) unit alts

-- | Process a set element with parent and side (State monad)
doSet :: forall t. Ord t => Eq t => Show t => Lem t -> Kid -> Side -> State (Tuple Kid (Kube t)) Unit
doSet L0 _ _ = pure unit
doSet (L1 value) parent side =
  modifyBuilder (addIndex value parent side)
doSet (Choice fst snd rest) parent side =
  doChoice (fst : snd : rest) parent side
doSet (Pair left right) parent _ = do
  keyId <- nextKid
  doSet left keyId Key
  modifyBuilder (addIndexRef keyId parent Key)
  doSet right keyId Val
doSet sek@(Sek _ _ _) parent side = do
  sid <- nextKid
  doSek sek sid
  modifyBuilder (addIndexRef sid parent side)
doSet doc parent side = do
  sid <- nextKid
  doDoc doc sid
  modifyBuilder (addIndexRef sid parent side)

-- | Helper to add to appropriate index
-- | Add data value to index
addIndex :: forall a. Ord a => a -> Kid -> Side -> Kube a -> Kube a
addIndex key value side builder = case side of
  Key -> builder { keys = put key value builder.keys }
  Val -> builder { vals = put key value builder.vals }
  Root -> addRoot value builder
  Sets -> addToSets value builder

-- | Add Kid reference to index
addIndexRef :: forall a. Ord a => Kid -> Kid -> Side -> Kube a -> Kube a
addIndexRef key value side builder = case side of
  Key -> builder { refKeys = put key value builder.refKeys }
  Val -> builder { refVals = put key value builder.refVals }
  Root -> addRoot value builder
  Sets -> addToSets value builder

-- Helper functions

padSeqs :: forall a. Ord a => Bi a -> Int -> Array (Bi a) -> Array (Bi a)
padSeqs emptyBi targetSize arr =
  if Array.length arr >= targetSize then arr
  else padSeqs emptyBi targetSize (Array.snoc arr emptyBi)