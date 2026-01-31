module Kubrick.Matcher(match) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl, all)
import Data.List.Lazy (List, fromFoldable, concat)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\), type (/\))
import Kubrick.Kube.Types (Kid, Kube, getValues, getKeys)
import Kubrick.Lem (Lem(..), Dict1(..))
import Kubrick.Types (Raw)

{-
Match returns all Kids that satisfy the input Lem (partial, non-strict matching).

Input → Matches:
• L1 a → L1(a) | Sek starting with a | Bag/Choice containing a
• Sek [a,b,..] → Sek/Sekdict with prefix [a,b,..]
• Bag {a,b,..} → Bag/Choice/[Bag|Choice]dict containing {a,b,..} as subset
• Choice(x,y,..) → Kids matching ANY of x, y, .. (OR logic)
• Pair(k,v) → Pair(k,v) | Dict/[Sek|Bag|Choice]dict containing (k,v)
• Dict{(k1,v1),(k2,v2),..} → Dict/[Sek|Bag|Choice]dict containing all pairs

Non-Dict elements use AND logic. Choice elements use OR logic.
-} 

match :: Kube -> Lem Raw -> List Kid
match kube query = case query of
  L0 -> mempty
  L1 a -> matchL1 a kube
  Sek lem1 lem2 rest -> matchSek (Sek lem1 lem2 rest) kube
  Bag lem1 lem2 rest -> matchBag (Bag lem1 lem2 rest) kube
  Choice lem1 lem2 rest -> concat $ fromFoldable 
    [ match kube lem1
    , match kube lem2
    , concat $ fromFoldable $ map (\l -> match kube l) rest
    ]
  Pair lem1 lem2 -> matchPair lem1 lem2 kube
  Dict pair1 pair2 rest -> matchDict (Dict pair1 pair2 rest) kube
  Bagdict _ dict -> matchDict' dict kube
  Sekdict _ dict -> matchDict' dict kube

-- Helper: Convert Set to List
toList :: forall a. Set a -> List a
toList = fromFoldable

-- Helper: Get values function from seqs array at given position
getSeqBiAt :: Int -> Kube -> Maybe (Raw -> Set Kid)
getSeqBiAt pos kube = map getValues (Array.index kube.seqs pos)

-- Helper: Find Kids where both key and value match (for Pairs)
findPairKids :: Raw -> Raw -> Kube -> Set Kid
findPairKids k v kube = 
  Set.intersection (getValues kube.keys k) (getValues kube.vals v)

-- Helper: Check if Kid is a Bag/Choice (in keys but not a Pair/Dict)
isBagOrChoice :: Kid -> Kube -> Boolean
isBagOrChoice kid kube = Set.isEmpty (getKeys kube.vals kid)

-- Helper: Get parent Kids that reference a given pairKid via refKeys
getParentKids :: Kid -> Kube -> Set Kid
getParentKids pairKid kube = getValues kube.refKeys pairKid

matchL1 :: Raw -> Kube -> List Kid
matchL1 a kube = toList matches
  where
    -- Match L1 at position 0 in Sek structures
    seqMatches = case getSeqBiAt 0 kube of
      Just getVals -> getVals a
      Nothing -> Set.empty
    
    -- Match L1 in Bag/Choice structures (in keys, but not Pair/Dict)
    -- Filter only if needed (non-empty candidates)
    keysMatches = getValues kube.keys a
    bagChoiceMatches = 
      if Set.isEmpty keysMatches 
        then Set.empty
        else Set.filter (\k -> isBagOrChoice k kube) keysMatches
    
    matches = Set.union seqMatches bagChoiceMatches

matchSek :: Lem Raw -> Kube -> List Kid
matchSek sek kube = 
  if allL1 sek
    then matchSekL1 sek kube
    else matchSekComposite sek kube
  where
    allL1 (Sek l1 l2 rest) = isL1 l1 && isL1 l2 && all isL1 rest
    allL1 _ = false
    isL1 (L1 _) = true
    isL1 _ = false

-- Match Sek with only L1 elements
matchSekL1 :: Lem Raw -> Kube -> List Kid
matchSekL1 sek kube = toList $ matchSekL1Core sek kube true

-- Core Sek L1 matching with optional roots filtering and parent discovery
matchSekL1Core :: Lem Raw -> Kube -> Boolean -> Set Kid
matchSekL1Core sek kube withRootsFilter = allMatches
  where
    elements = collectElements sek
    numElements = Array.length elements
    
    -- Get candidates from first element at position 0
    candidates = case Array.index elements 0 of
      Just firstElem -> case getSeqBiAt 0 kube of
        Just getVals -> getVals firstElem
        Nothing -> Set.empty
      Nothing -> Set.empty
    
    -- Check if Kid has all elements at consecutive positions (early termination)
    hasPrefix kid = go 0
      where
        go idx 
          | idx >= numElements = true
          | otherwise = case Array.index elements idx of
              Just elem -> case getSeqBiAt idx kube of
                Just getVals -> 
                  if Set.member kid (getVals elem)
                    then go (idx + 1)
                    else false
                Nothing -> false
              Nothing -> false
    
    directMatches = Set.filter hasPrefix candidates
    
    allMatches = if withRootsFilter
      then
        let parentMatches = findParentSeks (Set.toUnfoldable directMatches :: Array Kid) kube
        in Set.filter (\k -> Set.member k kube.roots) (Set.union directMatches parentMatches)
      else directMatches

-- Find parent Seks that contain the given Kids at the correct positions via refSeqs
findParentSeks :: Array Kid -> Kube -> Set Kid
findParentSeks matchedKids kube = 
  case matchedKids of
    [] -> Set.empty
    _ ->
      -- Get candidates from first matched Kid at position 0
      let candidates = case Array.index kube.refSeqs 0 of
            Just refBi -> 
              foldl Set.union Set.empty $ map (\k -> getValues refBi k) matchedKids
            Nothing -> Set.empty
          
          -- Check if parent has one of the matched Kids at position 0
          hasMatchedKidAtPos0 parentKid = case Array.index kube.refSeqs 0 of
            Just refBi ->
              let kidsAtPos0 = getKeys refBi parentKid
              in not Set.isEmpty (Set.intersection (Set.fromFoldable matchedKids) kidsAtPos0)
            Nothing -> false
      in Set.filter hasMatchedKidAtPos0 candidates

-- Match Sek with composite elements
matchSekComposite :: Lem Raw -> Kube -> List Kid
matchSekComposite sek kube = toList $ matchSekCompositeCore sek kube true

-- Core Sek composite matching with optional roots filtering
matchSekCompositeCore :: Lem Raw -> Kube -> Boolean -> Set Kid
matchSekCompositeCore sek kube withRootsFilter = allMatches
  where
    elements = collectSekElements sek
    numElements = Array.length elements
    
    -- Recursively match elements to get their Kids (use matchDirect to get internal Kids)
    elementKids = map (\lem -> matchDirect lem kube) elements
    
    -- Get candidates from first element at position 0
    candidates = case Array.index elementKids 0 of
      Just firstKids -> 
        if Set.isEmpty firstKids
          then Set.empty
          else case Array.index kube.refSeqs 0 of
            Just refBi -> 
              foldl Set.union Set.empty $ map (\k -> getValues refBi k) (Set.toUnfoldable firstKids :: Array Kid)
            Nothing -> Set.empty
      Nothing -> Set.empty
    
    -- Check if Kid has all element Kids at consecutive positions (early termination)
    hasPrefix kid = go 0
      where
        go idx 
          | idx >= numElements = true
          | otherwise = case Array.index elementKids idx of
              Just kidSet -> case Array.index kube.refSeqs idx of
                Just refBi -> 
                  let kidsAtPos = getKeys refBi kid
                  in if not Set.isEmpty (Set.intersection kidSet kidsAtPos)
                    then go (idx + 1)
                    else false
                Nothing -> false
              Nothing -> false
    
    directMatches = Set.filter hasPrefix candidates
    allMatches = if withRootsFilter
      then Set.filter (\k -> Set.member k kube.roots) directMatches
      else directMatches

-- Internal match function that returns direct Kids without roots filtering
matchDirect :: Lem Raw -> Kube -> Set Kid
matchDirect query kube = case query of
  L0 -> Set.empty
  L1 a -> matchL1Direct a kube
  Sek lem1 lem2 rest -> 
    if allL1Sek (Sek lem1 lem2 rest)
      then matchSekL1Direct (Sek lem1 lem2 rest) kube
      else matchSekCompositeDirect (Sek lem1 lem2 rest) kube
  Bag lem1 lem2 rest ->
    if allL1Bag (Bag lem1 lem2 rest)
      then matchBagL1Direct (Bag lem1 lem2 rest) kube
      else matchBagCompositeDirect (Bag lem1 lem2 rest) kube
  _ -> Set.empty
  where
    allL1Sek (Sek l1 l2 rest) = isL1 l1 && isL1 l2 && all isL1 rest
    allL1Sek _ = false
    allL1Bag (Bag l1 l2 rest) = isL1 l1 && isL1 l2 && all isL1 rest
    allL1Bag _ = false
    isL1 (L1 _) = true
    isL1 _ = false

matchL1Direct :: Raw -> Kube -> Set Kid
matchL1Direct a kube = 
  let seqMatches = case getSeqBiAt 0 kube of
        Just getVals -> getVals a
        Nothing -> Set.empty
      keysMatches = getValues kube.keys a
      bagChoiceMatches = 
        if Set.isEmpty keysMatches 
          then Set.empty
          else Set.filter (\k -> isBagOrChoice k kube) keysMatches
  in Set.union seqMatches bagChoiceMatches

matchSekL1Direct :: Lem Raw -> Kube -> Set Kid
matchSekL1Direct sek kube = matchSekL1Core sek kube false

matchSekCompositeDirect :: Lem Raw -> Kube -> Set Kid
matchSekCompositeDirect sek kube = matchSekCompositeCore sek kube false

matchBagL1Direct :: Lem Raw -> Kube -> Set Kid
matchBagL1Direct bag kube = matchBagL1Core bag kube false

matchBagCompositeDirect :: Lem Raw -> Kube -> Set Kid
matchBagCompositeDirect bag kube = matchBagCompositeCore bag kube false

-- Collect Lem elements from Sek (not decomposed to L1)
collectSekElements :: Lem Raw -> Array (Lem Raw)
collectSekElements = collect []
  where
    collect acc (Sek l1 l2 rest) = foldl (\a l -> Array.snoc a l) (Array.snoc (Array.snoc acc l1) l2) rest
    collect acc other = Array.snoc acc other

-- Collect L1 elements from Lem recursively
collectElements :: Lem Raw -> Array Raw
collectElements = collect []
  where
    collect acc (L1 a) = Array.snoc acc a
    collect acc (Sek l1 l2 rest) = foldl collect (collect (collect acc l1) l2) rest
    collect acc (Bag l1 l2 rest) = foldl collect (collect (collect acc l1) l2) rest
    collect acc (Choice l1 l2 rest) = foldl collect (collect (collect acc l1) l2) rest
    collect acc _ = acc

matchBag :: Lem Raw -> Kube -> List Kid
matchBag bag kube = 
  if allL1 bag
    then matchBagL1 bag kube
    else matchBagComposite bag kube
  where
    allL1 (Bag l1 l2 rest) = isL1 l1 && isL1 l2 && all isL1 rest
    allL1 _ = false
    isL1 (L1 _) = true
    isL1 _ = false

-- Match Bag with only L1 elements
matchBagL1 :: Lem Raw -> Kube -> List Kid
matchBagL1 bag kube = toList $ matchBagL1Core bag kube true

-- Core Bag L1 matching with optional roots filtering and parent discovery
matchBagL1Core :: Lem Raw -> Kube -> Boolean -> Set Kid
matchBagL1Core bag kube withRootsFilter = allMatches
  where
    elements = collectElements bag
    
    -- Get candidates from first element
    candidates = case Array.head elements of
      Just firstElem -> getValues kube.keys firstElem
      Nothing -> Set.empty
    
    -- Check if Kid has all bag elements in its keys (early termination on first miss)
    containsAll kid = go 0
      where
        numElements = Array.length elements
        go idx
          | idx >= numElements = true
          | otherwise = case Array.index elements idx of
              Just elem -> 
                if Set.member kid (getValues kube.keys elem)
                  then go (idx + 1)
                  else false
              Nothing -> false
    
    directMatches = Set.filter containsAll candidates
    
    allMatches = if withRootsFilter
      then
        let parentMatches = findParentBags (Set.toUnfoldable directMatches :: Array Kid) kube
        in Set.filter (\k -> Set.member k kube.roots) (Set.union directMatches parentMatches)
      else directMatches

-- Find parent Bags that contain the given Kids via refKeys
findParentBags :: Array Kid -> Kube -> Set Kid
findParentBags matchedKids kube = 
  case matchedKids of
    [] -> Set.empty
    _ ->
      -- Get all parents that reference these Kids
      let candidates = foldl Set.union Set.empty $ map (\k -> getValues kube.refKeys k) matchedKids
          -- Filter to only Bags (in sets)
      in Set.filter (\k -> Set.member k kube.sets) candidates

-- Match Bag with composite elements
matchBagComposite :: Lem Raw -> Kube -> List Kid
matchBagComposite bag kube = toList $ matchBagCompositeCore bag kube true

-- Core Bag composite matching with optional roots filtering and parent discovery
matchBagCompositeCore :: Lem Raw -> Kube -> Boolean -> Set Kid
matchBagCompositeCore bag kube withRootsFilter = allMatches
  where
    elements = collectBagElementLems bag
    numElements = Array.length elements
    
    -- Recursively match elements to get their Kids (use matchDirect to get internal Kids)
    elementKids = map (\lem -> matchDirect lem kube) elements
    
    -- Get candidates from first element
    candidates = case Array.index elementKids 0 of
      Just firstKids -> 
        if Set.isEmpty firstKids
          then Set.empty
          else 
            foldl Set.union Set.empty $ map (\k -> getValues kube.refKeys k) (Set.toUnfoldable firstKids :: Array Kid)
      Nothing -> Set.empty
    
    -- Check if Kid references all element Kids via refKeys (early termination on first miss)
    containsAll kid = go 0
      where
        referencedKids = getKeys kube.refKeys kid
        go idx
          | idx >= numElements = true
          | otherwise = case Array.index elementKids idx of
              Just kidSet -> 
                if not Set.isEmpty (Set.intersection kidSet referencedKids)
                  then go (idx + 1)
                  else false
              Nothing -> false
    
    directMatches = Set.filter (\kid -> Set.member kid kube.sets && containsAll kid) candidates
    
    allMatches = if withRootsFilter
      then
        let parentMatches = findParentBags (Set.toUnfoldable directMatches :: Array Kid) kube
        in Set.filter (\k -> Set.member k kube.roots) (Set.union directMatches parentMatches)
      else directMatches

-- Collect Lem elements from Bag (not decomposed to L1)
collectBagElementLems :: Lem Raw -> Array (Lem Raw)
collectBagElementLems = collect []
  where
    collect acc (Bag l1 l2 rest) = foldl (\a l -> Array.snoc a l) (Array.snoc (Array.snoc acc l1) l2) rest
    collect acc other = Array.snoc acc other

matchPair :: Lem Raw -> Lem Raw -> Kube -> List Kid
matchPair keyLem valLem kube = case keyLem, valLem of
  L1 k, L1 v -> toList matches
    where
      pairKids = findPairKids k v kube
      
      -- Early exit if no pairKids found
      matches = if Set.isEmpty pairKids
        then Set.empty
        else
          let
            -- Standalone Pairs are in roots
            standalonePairs = Set.filter (\kid -> Set.member kid kube.roots) pairKids
            
            -- Parent Dicts reference pair Kids via refKeys (cache array conversion)
            pairKidsArray = Set.toUnfoldable pairKids :: Array Kid
            parentDicts = foldl Set.union Set.empty $ 
              map (\pk -> getParentKids pk kube) pairKidsArray
          in Set.union standalonePairs parentDicts
  _, _ -> mempty

matchDict :: Lem Raw -> Kube -> List Kid
matchDict dict kube = toList matches
  where
    pairs = collectPairs dict
    
    -- Find pairKids for each queried pair
    pairKidSets = map (\(k /\ v) -> findPairKids k v kube) pairs
    numPairs = Array.length pairKidSets
    
    -- Get candidate parent Kids from first pair
    candidates = case Array.head pairKidSets of
      Just firstSet -> 
        if Set.isEmpty firstSet
          then Set.empty
          else
            let firstSetArray = Set.toUnfoldable firstSet :: Array Kid
            in foldl Set.union Set.empty $ map (\pk -> getParentKids pk kube) firstSetArray
      Nothing -> Set.empty
    
    -- Parent must reference all queried pairs (early termination, cache referenced pairs)
    referencesAllPairs parentKid = 
      let referencedPairs = getKeys kube.refKeys parentKid
      in checkAll 0 referencedPairs
      where
        checkAll idx refPairs
          | idx >= numPairs = true
          | otherwise = case Array.index pairKidSets idx of
              Just pairSet ->
                if not Set.isEmpty (Set.intersection refPairs pairSet)
                  then checkAll (idx + 1) refPairs
                  else false
              Nothing -> false
    
    matches = Set.filter referencesAllPairs candidates

-- Collect key-value pairs from Dict structures
collectPairs :: forall a. Lem a -> Array (a /\ a)
collectPairs = collect []
  where
    collect acc (Pair (L1 k) (L1 v)) = Array.snoc acc (k /\ v)
    collect acc (Dict p1 p2 rest) = 
      let withFirst = collectFromTuple acc p1
          withSecond = collectFromTuple withFirst p2
      in foldl collectFromTuple withSecond rest
    collect acc _ = acc
    
    collectFromTuple acc (Tuple (L1 k) (L1 v)) = Array.snoc acc (k /\ v)
    collectFromTuple acc _ = acc

matchDict' :: Dict1 Raw -> Kube -> List Kid
matchDict' dict kube = case dict of
  D1 (L1 k) (L1 v) -> matchPair (L1 k) (L1 v) kube
  D2 p1 p2 rest -> matchDict (Dict p1 p2 rest) kube
  _ -> mempty