module Kubrick.Builder (add) where

import Prelude

import Control.Monad.State (State, state)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldM)
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Kubrick.Kube.Types (Kid(..), Bi, Kube, bi0, put)
import Kubrick.Lem (Lem(..), Sek1(..), Bag1(..), Dict1(..))
import Kubrick.Types (Raw)

-- | Add a document to the builder using State monad
-- | Returns the Kid that represents this Lem structure  
add :: Lem Raw -> State (Tuple Kid Kube) Kid
add doc = case doc of
  -- Dict needs special handling: pairs get Kids first, then Dict gets its Kid
  Dict fst snd rest -> do
    dictKid <- doMapAndReturnKid (fst : snd : rest)
    modifyBuilder (addRoot dictKid)
    pure dictKid
  -- Sek with nested Lems needs special handling: nested elements get Kids first
  Sek fst snd rest -> do
    sekKid <- doSekAndReturnKid (fst : snd : rest)
    modifyBuilder (addRoot sekKid)
    pure sekKid
  -- For all other Lems, allocate Kid first then process
  _ -> do
    kid <- nextKid
    doDoc doc kid
    modifyBuilder (addRoot kid)
    pure kid

-- | Side enum representing different index types
data Side
  = Key
  | Val
  | Root
  | Sets

derive instance eqSide :: Eq Side
derive instance ordSide :: Ord Side
-- | Process a Sek and return its Kid (allocated after nested elements)
-- | This ensures nested Seks get lower Kids than their parent
doSekAndReturnKid :: List (Lem Raw) -> State (Tuple Kid Kube) Kid
doSekAndReturnKid elements = do
  -- Allocate Kids for nested elements first by processing the line with a temporary parent
  -- We'll collect info about what needs to be linked
  indexed <- pure $ List.zipWith Tuple (List.range 0 (List.length elements - 1)) elements
  -- Process elements and collect their info
  elementInfo <- traverse (\(Tuple position elem) -> do
    case elem of
      L1 value -> pure (Tuple position (Left value))
      Sek fst snd rest -> do
        nid <- nextKid
        doSek (Sek fst snd rest) nid
        pure (Tuple position (Right nid))
      other -> do
        nid <- nextKid
        doDoc other nid
        pure (Tuple position (Right nid))
  ) indexed
  -- Now allocate the Sek Kid
  sekKid <- nextKid
  -- Add the element info to the Sek
  void $ traverse (\(Tuple position info) -> case info of
    Left value -> modifyBuilder (addPos position value sekKid)
    Right elemKid -> modifyBuilder (addPosRef position elemKid sekKid)
  ) elementInfo
  pure sekKid

-- | Process a map and return its Kid (allocated after pairs)
doMapAndReturnKid :: List (Tuple (Lem Raw) (Lem Raw)) -> State (Tuple Kid Kube) Kid
doMapAndReturnKid entries = do
  -- First, allocate Kids for all pairs
  pairKids <- traverse (\(Tuple key value) -> do
    pairKid <- nextKid
    doPair key value pairKid
    pure pairKid
  ) entries
  -- Now allocate the Dict Kid
  dictKid <- nextKid
  -- Link all pair Kids to Dict Kid via refKeys
  void $ traverse (\pairKid -> modifyBuilder (addIndexRef pairKid dictKid Key)) pairKids
  modifyBuilder (addToSets dictKid)
  pure dictKid

-- | Add positioned data value
addPos :: Int -> Raw -> Kid -> Kube -> Kube
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
addPosRef :: Int -> Kid -> Kid -> Kube -> Kube
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
addRoot :: Kid -> Kube -> Kube
addRoot value builder =
  builder { roots = Set.insert value builder.roots }

-- | Add to sets set
addToSets :: Kid -> Kube -> Kube
addToSets value builder =
  builder { sets = Set.insert value builder.sets }

-- | Get next Kid and increment counter
nextKid :: State (Tuple Kid Kube) Kid
nextKid = state \(Tuple (Kid n) builder) -> Tuple (Kid n) (Tuple (Kid (n + 1)) builder)

-- | Modify builder
modifyBuilder :: (Kube -> Kube) -> State (Tuple Kid Kube) Unit
modifyBuilder f = state \(Tuple kid builder) -> Tuple unit (Tuple kid (f builder))

-- | Process a document based on its type (State monad version)
doDoc :: Lem Raw -> Kid -> State (Tuple Kid Kube) Unit
doDoc L0 _ = pure unit
doDoc (L1 value) kid = modifyBuilder (addPos 0 value kid)
doDoc (Choice fst snd rest) kid = doChoice (fst : snd : rest) kid Key
doDoc (Pair k v) kid = doPair k v kid
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

-- | Process a Pair - both key and val map to the SAME Kid
doPair :: Lem Raw -> Lem Raw -> Kid -> State (Tuple Kid Kube) Unit
doPair k v kid = do
  doPairElement k kid Key
  doPairElement v kid Val

-- | Process a Pair element (key or value)
doPairElement :: Lem Raw -> Kid -> Side -> State (Tuple Kid Kube) Unit
doPairElement L0 _ _ = pure unit
doPairElement (L1 value) kid side =
  modifyBuilder (addIndex value kid side)
doPairElement (Choice fst snd rest) kid side =
  doChoice (fst : snd : rest) kid side
doPairElement doc kid side = do
  nid <- nextKid
  doDoc doc nid
  modifyBuilder (addIndexRef nid kid side)

-- | Process Sek1 types (State monad)
doSekFromSek1 :: Sek1 Raw -> Kid -> State (Tuple Kid Kube) Unit
doSekFromSek1 (S1 lem) kid = doDoc lem kid
doSekFromSek1 (S2 fst snd rest) kid = doSek (Sek fst snd rest) kid

-- | Process Bag1 types (State monad)
doBagFromBag1 :: Bag1 Raw -> Kid -> State (Tuple Kid Kube) Unit
doBagFromBag1 (B1 lem) kid = doDoc lem kid
doBagFromBag1 (B2 fst snd rest) kid = doChoice (fst : snd : rest) kid Key

-- | Process Dict1 types (State monad)
-- | For Sekdict/Bagdict, the parent Kid is already allocated
-- | We need to create pair Kids and link them to the parent
doDictFromDict1 :: Dict1 Raw -> Kid -> State (Tuple Kid Kube) Unit
doDictFromDict1 (D1 k v) parentKid = do
  -- The Pair needs its own Kid, then link to parent via refKeys
  pairKid <- nextKid
  doPair k v pairKid
  modifyBuilder (addIndexRef pairKid parentKid Key)
  modifyBuilder (addToSets parentKid)
doDictFromDict1 (D2 fst snd rest) parentKid = do
  -- Create pairs and link to parent
  let entries = fst : snd : rest
  void $ traverse (\(Tuple key value) -> do
    pairKid <- nextKid
    doPair key value pairKid
    modifyBuilder (addIndexRef pairKid parentKid Key)
  ) entries
  modifyBuilder (addToSets parentKid)

-- | Process a Sek structure (State monad)
doSek :: Lem Raw -> Kid -> State (Tuple Kid Kube) Unit
doSek (Sek fst snd rest) kid =
  let
    allElems = fst : snd : rest
  in
    doLine allElems kid
doSek lem kid = doDoc lem kid

-- | Process a line (array sequence) (State monad)
doLine :: List (Lem Raw) -> Kid -> State (Tuple Kid Kube) Unit
doLine line kid =
  let
    indexed :: List (Tuple Int (Lem Raw))
    indexed = List.zip (List.range 0 (List.length line - 1)) line
  in
    void $ foldM (\_ (Tuple position doc) -> doLineElement doc position kid) unit indexed

-- | Process a single line element (State monad)
doLineElement :: Lem Raw -> Int -> Kid -> State (Tuple Kid Kube) Unit
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
-- | Each pair gets its own Kid, then the Dict Kid references them via refKeys
doMap :: List (Tuple (Lem Raw) (Lem Raw)) -> Kid -> State (Tuple Kid Kube) Unit
doMap entries parent = do
  void $ foldM (\_ entry -> doMapEntry parent entry) unit entries
  modifyBuilder (addToSets parent)

-- | Process a single map entry (State monad)
-- | Creates a new Kid for the pair and links it to parent via refKeys
doMapEntry :: Kid -> Tuple (Lem Raw) (Lem Raw) -> State (Tuple Kid Kube) Unit
doMapEntry parent (Tuple key value) = do
  pairKid <- nextKid
  doPair key value pairKid
  modifyBuilder (addIndexRef pairKid parent Key)

-- | Process a choice (alternatives) (State monad)
doChoice :: List (Lem Raw) -> Kid -> Side -> State (Tuple Kid Kube) Unit
doChoice alts parent side =
  void $ foldM (\_ doc -> doChoiceElement doc parent side) unit alts

-- | Process a single choice element
doChoiceElement :: Lem Raw -> Kid -> Side -> State (Tuple Kid Kube) Unit
doChoiceElement L0 _ _ = pure unit
doChoiceElement (L1 value) parent side =
  modifyBuilder (addIndex value parent side)
doChoiceElement (Choice fst snd rest) parent side =
  doChoice (fst : snd : rest) parent side
doChoiceElement sek@(Sek _ _ _) parent side = do
  sid <- nextKid
  doSek sek sid
  modifyBuilder (addIndexRef sid parent side)
doChoiceElement doc parent side = do
  sid <- nextKid
  doDoc doc sid
  modifyBuilder (addIndexRef sid parent side)

-- | Helper to add to appropriate index
-- | Add data value to index
addIndex :: Raw -> Kid -> Side -> Kube -> Kube
addIndex key value side builder = case side of
  Key -> builder { keys = put key value builder.keys }
  Val -> builder { vals = put key value builder.vals }
  Root -> addRoot value builder
  Sets -> addToSets value builder

-- | Add Kid reference to index
addIndexRef :: Kid -> Kid -> Side -> Kube -> Kube
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