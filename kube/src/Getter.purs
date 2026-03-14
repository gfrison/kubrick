module Kubrick.Getter
  ( get
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\), type (/\))
import Kubrick.Lem (Lem(..), (<+>), (<+))
import Kubrick.Kube.Types (Kid, Bi, Kube, getKeys)

-- | Reconstruct a Lem from a Kid in a Kube
-- | This is the inverse operation of Builder.add
get :: forall a. Eq a => Ord a => Kube a -> Kid -> Maybe (Lem a)
get kube kid = reconstructLem Set.empty kid kube

-- | Main reconstruction logic with cycle detection
reconstructLem :: forall a. Eq a => Ord a => Set.Set Kid -> Kid -> Kube a -> Maybe (Lem a)
reconstructLem visited kid kube
  | Set.member kid visited = Nothing  -- Cycle detected
  | otherwise =
      let
        visited' = Set.insert kid visited
        -- Check if this Kid has sequence data
        seqLem = reconstructSeq visited' kid kube
        
        -- Check if this Kid has key/val data (Pair or Choice/Bag)
        keyValLem = reconstructKeyVal visited' kid kube
      in
        case seqLem /\ keyValLem of
          -- Only sequence
          Just sek /\ Nothing -> Just sek
          
          -- Only key-val
          Nothing /\ Just kvLem -> Just kvLem
          
          -- Both sequence and key-val: need to determine how to combine
          Just sek /\ Just kvLem -> Just (combineSekWithKeyVal sek kvLem)
          
          -- No data found
          Nothing /\ Nothing -> Nothing

-- | Combine sequence data with key-val data intelligently
-- | If the key-val is a simple value (L1), append it to the sequence
-- | Otherwise, combine as a Bag
combineSekWithKeyVal :: forall a. Eq a => Lem a -> Lem a -> Lem a
combineSekWithKeyVal sek (L1 value) = value <+ sek  -- Prepend to create Bag
combineSekWithKeyVal sek kvLem = sek <+> kvLem      -- Combine as Bag

-- | Reconstruct sequence (Sek) from seqs array
reconstructSeq :: forall a. Eq a => Ord a => Set.Set Kid -> Kid -> Kube a -> Maybe (Lem a)
reconstructSeq visited kid kube =
  let
    -- Collect all elements from all sequence positions
    elements = collectSeqElements visited kid kube
  in
    case elements of
      [] -> Nothing
      [x] -> Just x
      _ -> 
        case List.fromFoldable elements of
          x : y : rest -> Just (Sek x y rest)
          x : Nil -> Just x
          Nil -> Nothing

-- | Collect elements from sequence positions for a specific Kid
collectSeqElements :: forall a. Eq a => Ord a => Set.Set Kid -> Kid -> Kube a -> Array (Lem a)
collectSeqElements visited kid kube =
  let
    -- Get data values from regular seqs (with original positions preserved)
    dataElems = Array.mapWithIndex (\idx bi -> getDataElemAtPos idx bi kid) kube.seqs
    -- Get Kid references from refSeqs (with original positions preserved)
    refElems = Array.mapWithIndex (\idx bi -> getRefElemAtPos visited idx bi kid kube) kube.refSeqs
  in
    -- Merge both, prioritizing refs over data at same position (refs are complex structures)
    mergeSeqElements dataElems refElems

-- | Merge data and ref Maybe elements, combining both arrays at their original positions
mergeSeqElements :: forall a. Array (Maybe (Lem a)) -> Array (Maybe (Lem a)) -> Array (Lem a)
mergeSeqElements dataElems refElems =
  let
    maxLen = max (Array.length dataElems) (Array.length refElems)
  in
    Array.mapMaybe identity $
      Array.mapWithIndex (\idx _ ->
        case Array.index refElems idx /\ Array.index dataElems idx of
          Just (Just refElem) /\ _ -> Just refElem -- Prefer ref (complex structure)
          _ /\ Just (Just dataElem) -> Just dataElem
          _ -> Nothing
      ) (Array.range 0 (maxLen - 1))

-- | Get data element at a specific position
getDataElemAtPos :: forall a. Ord a => Int -> Bi a -> Kid -> Maybe (Lem a)
getDataElemAtPos _ bi kid =
  let
    values = Set.toUnfoldable (getKeys bi kid)
  in
    Array.head values <#> L1

-- | Get ref element at a specific position
getRefElemAtPos :: forall a. Eq a => Ord a => Set.Set Kid -> Int -> Bi Kid -> Kid -> Kube a -> Maybe (Lem a)
getRefElemAtPos visited _ bi targetKid kube =
  let
    kidRefs = Set.toUnfoldable (getKeys bi targetKid) :: Array Kid
  in
    case Array.head kidRefs of
      Nothing -> Nothing
      Just refKid -> reconstructLem visited refKid kube


-- | Reconstruct key-value structure (Choice, Bag, Pair)
reconstructKeyVal :: forall a. Eq a => Ord a => Set.Set Kid -> Kid -> Kube a -> Maybe (Lem a)
reconstructKeyVal visited kid kube =
  let
    -- Get data values
    keyDataValues = Set.toUnfoldable (getKeys kube.keys kid)
    valDataValues = Set.toUnfoldable (getKeys kube.vals kid)
    -- Get Kid references
    keyRefValues = Set.toUnfoldable (getKeys kube.refKeys kid) :: Array Kid
    valRefValues = Set.toUnfoldable (getKeys kube.refVals kid) :: Array Kid
    isSet = Set.member kid kube.sets
    
    -- Reconstruct data values to Lems
    keyDataLems = keyDataValues <#> L1
    valDataLems = valDataValues <#> L1
    -- Reconstruct Kid references to Lems
    keyRefLems = Array.mapMaybe (\k -> reconstructLem visited k kube) keyRefValues
    valRefLems = Array.mapMaybe (\k -> reconstructLem visited k kube) valRefValues
    
    -- Combine data and ref Lems
    allKeyLems = keyDataLems <> keyRefLems
    allValLems = valDataLems <> valRefLems
  in
    case Array.length allKeyLems /\ Array.length allValLems of
      -- Has both keys and vals -> Pair or Dict
      kl /\ vl | kl > 0 && vl > 0 -> reconstructPairFromLems allKeyLems allValLems
      
      -- Only keys - check if they're all Pairs (Dict case)
      kl /\ 0 | kl > 0 ->
        case extractPairs allKeyLems of
          Just pairs -> reconstructDictFromPairs pairs
          Nothing ->
            -- Not all Pairs, check if it's a set
            if isSet
              then reconstructBagFromLems allKeyLems
              else reconstructChoiceFromLems allKeyLems
      
      -- No data
      _ -> Nothing

-- | Extract key-value tuples from Pair Lems
extractPairs :: forall a. Array (Lem a) -> Maybe (Array (Tuple (Lem a) (Lem a)))
extractPairs lems =
  let extracted = lems <#> \lem -> case lem of
        Pair k v -> Just (Tuple k v)
        _ -> Nothing
  in if Array.all (\x -> case x of
                          Just _ -> true
                          Nothing -> false) extracted
     then Just (Array.mapMaybe identity extracted)
     else Nothing

-- | Reconstruct a Dict from an array of key-value tuples
reconstructDictFromPairs :: forall a. Array (Tuple (Lem a) (Lem a)) -> Maybe (Lem a)
reconstructDictFromPairs pairs =
  case Array.uncons pairs of
    Nothing -> Nothing
    Just { head: t1, tail } ->
      case Array.uncons tail of
        Nothing -> case t1 of
          Tuple k v -> Just (Pair k v)
        Just { head: t2, tail: rest } ->
          Just (Dict t1 t2 (List.fromFoldable rest))

-- | Reconstruct a Pair or Dict from Lems
reconstructPairFromLems :: forall a. Array (Lem a) -> Array (Lem a) -> Maybe (Lem a)
reconstructPairFromLems keyLems valLems
  | Array.length keyLems /= Array.length valLems = Nothing
  | otherwise =
      case Array.zip keyLems valLems of
        [] -> Nothing
        [Tuple k v] -> Just (Pair k v)
        pairs ->
          case Array.uncons pairs of
            Nothing -> Nothing
            Just { head: Tuple k1 v1, tail } ->
              case Array.uncons tail of
                Nothing -> Just (Pair k1 v1)
                Just { head: Tuple k2 v2, tail: rest } ->
                  let tuples = rest <#> \(Tuple k v) -> Tuple k v
                  in Just (Dict (Tuple k1 v1) (Tuple k2 v2) (List.fromFoldable tuples))

-- | Reconstruct a Bag from Lems
reconstructBagFromLems :: forall a. Eq a => Array (Lem a) -> Maybe (Lem a)
reconstructBagFromLems lems =
  case List.fromFoldable lems of
    x : y : rest -> Just (foldl (<+>) (x <+> y) rest)
    x : Nil -> Just x
    Nil -> Nothing

-- | Reconstruct a Choice from Lems
reconstructChoiceFromLems :: forall a. Array (Lem a) -> Maybe (Lem a)
reconstructChoiceFromLems lems =
  case List.fromFoldable lems of
    x : y : rest -> Just (Choice x y rest)
    x : Nil -> Just x
    Nil -> Nothing
