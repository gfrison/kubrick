module Kubrick.Matcher(match) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl, all)
import Data.List.Lazy (List, fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\), type (/\))
import Kubrick.Kube.Types (Kid, Kube, getValues, getKeys)
import Kubrick.Lem (Lem(..), Dict1(..))

match :: forall a. Ord a => Kube a -> Lem a -> List Kid
match kube query = case query of
  L0 -> mempty
  Gap -> matchGap kube  -- Gap matches everything with 1 element
  L1 a -> matchL1 a kube
  Sek lem1 lem2 rest -> matchSek (Sek lem1 lem2 rest) kube
  Bag lem1 lem2 rest -> matchBag (Bag lem1 lem2 rest) kube
  Choice lem1 lem2 rest -> 
    let results = Set.fromFoldable (match kube lem1)
                <> Set.fromFoldable (match kube lem2)
                <> foldl (<>) Set.empty (map (\l -> Set.fromFoldable (match kube l)) rest)
    in toList results
  Pair lem1 lem2 -> matchPair lem1 lem2 kube
  Dict pair1 pair2 rest -> matchDict (Dict pair1 pair2 rest) kube
  Bagdict _ dict -> matchDict' dict kube
  Sekdict _ dict -> matchDict' dict kube

-- Helper: Convert Set to List
toList :: forall a. Set a -> List a
toList = fromFoldable

-- Helper: Get values function from seqs array at given position
getSeqBiAt :: forall a. Ord a => Int -> Kube a -> Maybe (a -> Set Kid)
getSeqBiAt pos kube = map getValues (Array.index kube.seqs pos)

-- Helper: Find Kids where both key and value match (for Pairs)
findPairKids :: forall a. Ord a => a -> a -> Kube a -> Set Kid
findPairKids k v kube = 
  Set.intersection (getValues kube.keys k) (getValues kube.vals v)

-- Helper: Check if Kid is a Bag/Choice (in keys but not a Pair/Dict)
isBagOrChoice :: forall a. Kid -> Kube a -> Boolean
isBagOrChoice kid kube = Set.isEmpty (getKeys kube.vals kid)

-- Helper: Get parent Kids that reference a given pairKid via refKeys
getParentKids :: forall a. Kid -> Kube a -> Set Kid
getParentKids pairKid kube = getValues kube.refKeys pairKid

-- Match Gap: returns all roots
matchGap :: forall a. Kube a -> List Kid
matchGap kube = toList kube.roots

matchL1 :: forall a. Ord a => a -> Kube a -> List Kid
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

matchSek :: forall a. Ord a => Lem a -> Kube a -> List Kid
matchSek sek kube = 
  if allL1OrGap sek
    then matchSekL1WithGap sek kube
    else matchSekComposite sek kube
  where
    allL1OrGap (Sek l1 l2 rest) = isL1OrGap l1 && isL1OrGap l2 && all isL1OrGap rest
    allL1OrGap _ = false
    isL1OrGap (L1 _) = true
    isL1OrGap Gap = true
    isL1OrGap _ = false

-- Match Sek with only L1 elements (and possibly Gap)
matchSekL1WithGap :: forall a. Ord a => Lem a -> Kube a -> List Kid
matchSekL1WithGap sek kube = toList $ matchSekL1WithGapCore sek kube true

-- Legacy function for backward compatibility (kept for potential future use)
_matchSekL1 :: forall a. Ord a => Lem a -> Kube a -> List Kid
_matchSekL1 sek kube = toList $ matchSekL1Core sek kube true

-- Core Sek L1 matching with Gap support
matchSekL1WithGapCore :: forall a. Ord a => Lem a -> Kube a -> Boolean -> Set Kid
matchSekL1WithGapCore sek kube withRootsFilter = allMatches
  where
    elements = collectElementsWithGap sek
    numElements = Array.length elements
    
    -- Get candidates from first non-Gap element
    candidates = findFirstNonGapCandidates elements kube
    
    -- Check if Kid has matching elements at positions (Gap matches anything)
    hasPrefix kid = go 0
      where
        go idx 
          | idx >= numElements = true
          | otherwise = case Array.index elements idx of
              Just (Just elem) -> case getSeqBiAt idx kube of
                Just getVals -> 
                  if Set.member kid (getVals elem)
                    then go (idx + 1)
                    else false
                Nothing -> false
              Just Nothing -> -- Gap at this position - check anything exists
                case getSeqBiAt idx kube of
                  Just _ -> go (idx + 1) -- Position exists, Gap matches
                  Nothing -> false -- Position doesn't exist
              Nothing -> false
    
    directMatches = Set.filter hasPrefix candidates
    
    allMatches = if withRootsFilter
      then
        let parentMatches = findParentSeks (Set.toUnfoldable directMatches :: Array Kid) kube
        in Set.filter (\k -> Set.member k kube.roots) (Set.union directMatches parentMatches)
      else directMatches

-- Core Sek L1 matching with optional roots filtering and parent discovery
matchSekL1Core :: forall a. Ord a => Lem a -> Kube a -> Boolean -> Set Kid
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
findParentSeks :: forall a. Array Kid -> Kube a -> Set Kid
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
matchSekComposite :: forall a. Ord a => Lem a -> Kube a -> List Kid
matchSekComposite sek kube = toList $ matchSekCompositeCore sek kube true

-- Core Sek composite matching with optional roots filtering
matchSekCompositeCore :: forall a. Ord a => Lem a -> Kube a -> Boolean -> Set Kid
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
matchDirect :: forall a. Ord a => Lem a -> Kube a -> Set Kid
matchDirect query kube = case query of
  L0 -> Set.empty
  Gap -> Set.empty
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

matchL1Direct :: forall a. Ord a => a -> Kube a -> Set Kid
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

matchSekL1Direct :: forall a. Ord a => Lem a -> Kube a -> Set Kid
matchSekL1Direct sek kube = matchSekL1Core sek kube false

matchSekCompositeDirect :: forall a. Ord a => Lem a -> Kube a -> Set Kid
matchSekCompositeDirect sek kube = matchSekCompositeCore sek kube false

matchBagL1Direct :: forall a. Ord a => Lem a -> Kube a -> Set Kid
matchBagL1Direct bag kube = matchBagL1Core bag kube false

matchBagCompositeDirect :: forall a. Ord a => Lem a -> Kube a -> Set Kid
matchBagCompositeDirect bag kube = matchBagCompositeCore bag kube false

-- Collect Lem elements from Sek (not decomposed to L1)
collectSekElements :: forall a. Lem a -> Array (Lem a)
collectSekElements = collect []
  where
    collect acc (Sek l1 l2 rest) = foldl (\a l -> Array.snoc a l) (Array.snoc (Array.snoc acc l1) l2) rest
    collect acc other = Array.snoc acc other

-- Collect L1 elements from Lem recursively
collectElements :: forall a. Lem a -> Array a
collectElements = collect []
  where
    collect acc (L1 a) = Array.snoc acc a
    collect acc (Sek l1 l2 rest) = foldl collect (collect (collect acc l1) l2) rest
    collect acc (Bag l1 l2 rest) = foldl collect (collect (collect acc l1) l2) rest
    collect acc (Choice l1 l2 rest) = foldl collect (collect (collect acc l1) l2) rest
    collect acc _ = acc

-- Collect elements with Gap (Nothing = Gap, Just Raw = L1)
collectElementsWithGap :: forall a. Lem a -> Array (Maybe a)
collectElementsWithGap = collect []
  where
    collect acc (L1 a) = Array.snoc acc (Just a)
    collect acc Gap = Array.snoc acc Nothing
    collect acc (Sek l1 l2 rest) = foldl collect (collect (collect acc l1) l2) rest
    collect acc (Bag l1 l2 rest) = foldl collect (collect (collect acc l1) l2) rest
    collect acc (Choice l1 l2 rest) = foldl collect (collect (collect acc l1) l2) rest
    collect acc _ = acc

-- Find candidates from first non-Gap element at its position
findFirstNonGapCandidates :: forall a. Ord a => Array (Maybe a) -> Kube a -> Set Kid
findFirstNonGapCandidates elements kube = go 0
  where
    numElements = Array.length elements
    go idx
      | idx >= numElements = 
          -- All Gaps - need to find all Kids with at least numElements positions
          -- Get all Kids that appear at position 0
          case Array.index kube.seqs 0 of
            Just bi ->
              let _allKidsAtPos0 = foldl (\acc raw -> Set.union acc (getValues bi raw)) Set.empty getAllRawValues
                  getAllRawValues = []  -- We need all possible Raw values, but we don't have them
                  -- Instead, we filter roots by checking if they have enough positions
              in Set.filter (hasEnoughPositions (numElements - 1)) kube.roots
            Nothing -> Set.empty
      | otherwise = case Array.index elements idx of
          Just (Just elem) -> case getSeqBiAt idx kube of
            Just getVals -> getVals elem
            Nothing -> Set.empty
          Just Nothing -> go (idx + 1) -- Skip Gap, try next
          Nothing -> Set.empty
    
    -- Check if Kid has positions up to maxPos
    hasEnoughPositions maxPos kid = checkPos 0
      where
        checkPos pos
          | pos > maxPos = true
          | otherwise = case Array.index kube.refSeqs pos of
              Just refBi -> 
                -- Check if this kid has any entry at this position
                not Set.isEmpty (getKeys refBi kid) || checkDataPos pos
              Nothing -> checkDataPos pos
        
        checkDataPos pos = case Array.index kube.seqs pos of
          Just dataBi ->
            not Set.isEmpty (getKeys dataBi kid)
          Nothing -> false

matchBag :: forall a. Ord a => Lem a -> Kube a -> List Kid
matchBag bag kube = 
  if allL1OrGap bag
    then matchBagL1WithGap bag kube
    else matchBagComposite bag kube
  where
    allL1OrGap (Bag l1 l2 rest) = isL1OrGap l1 && isL1OrGap l2 && all isL1OrGap rest
    allL1OrGap _ = false
    isL1OrGap (L1 _) = true
    isL1OrGap Gap = true
    isL1OrGap _ = false

-- Match Bag with L1 and Gap elements
matchBagL1WithGap :: forall a. Ord a => Lem a -> Kube a -> List Kid
matchBagL1WithGap bag kube = toList $ matchBagL1WithGapCore bag kube true

-- Match Bag with only L1 elements (kept for potential future use)
_matchBagL1 :: forall a. Ord a => Lem a -> Kube a -> List Kid
_matchBagL1 bag kube = toList $ matchBagL1Core bag kube true

-- Core Bag L1 with Gap matching - Gap means at least one more element
matchBagL1WithGapCore :: forall a. Ord a => Lem a -> Kube a -> Boolean -> Set Kid
matchBagL1WithGapCore bag kube withRootsFilter = allMatches
  where
    allElements = collectElementsWithGap bag
    elements = Array.mapMaybe identity allElements -- Filter out Gaps to get L1 elements
    numGaps = Array.length $ Array.filter (\x -> x == Nothing) allElements
    
    -- Get candidates from first element, or all keys if only Gaps
    candidates = case Array.head elements of
      Just firstElem -> getValues kube.keys firstElem
      Nothing -> 
        -- All Gaps - get all Bag/Choice Kids (those in keys but not vals)
        Set.filter (\k -> isBagOrChoice k kube) kube.roots
    
    -- Check if Kid has all bag elements plus at least as many extra as there are Gaps
    containsAll kid = 
      let kidKeys = getKeys kube.keys kid
          numKidKeys = Set.size kidKeys
          numRequiredElements = Array.length elements
          minTotalRequired = numRequiredElements + numGaps
          hasAllElements = go 0
            where
              go idx
                | idx >= numRequiredElements = true
                | otherwise = case Array.index elements idx of
                    Just elem -> 
                      if Set.member kid (getValues kube.keys elem)
                        then go (idx + 1)
                        else false
                    Nothing -> false
      in hasAllElements && numKidKeys >= minTotalRequired
    
    directMatches = Set.filter containsAll candidates
    
    allMatches = if withRootsFilter
      then
        let parentMatches = findParentBags (Set.toUnfoldable directMatches :: Array Kid) kube
        in Set.filter (\k -> Set.member k kube.roots) (Set.union directMatches parentMatches)
      else directMatches

-- Core Bag L1 matching with optional roots filtering and parent discovery
matchBagL1Core :: forall a. Ord a => Lem a -> Kube a -> Boolean -> Set Kid
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
findParentBags :: forall a. Array Kid -> Kube a -> Set Kid
findParentBags matchedKids kube = 
  case matchedKids of
    [] -> Set.empty
    _ ->
      -- Get all parents that reference these Kids
      let candidates = foldl Set.union Set.empty $ map (\k -> getValues kube.refKeys k) matchedKids
          -- Filter to only Bags (in sets)
      in Set.filter (\k -> Set.member k kube.sets) candidates

-- Match Bag with composite elements
matchBagComposite :: forall a. Ord a => Lem a -> Kube a -> List Kid
matchBagComposite bag kube = toList $ matchBagCompositeCore bag kube true

-- Core Bag composite matching with optional roots filtering and parent discovery
matchBagCompositeCore :: forall a. Ord a => Lem a -> Kube a -> Boolean -> Set Kid
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
collectBagElementLems :: forall a. Lem a -> Array (Lem a)
collectBagElementLems = collect []
  where
    collect acc (Bag l1 l2 rest) = foldl (\a l -> Array.snoc a l) (Array.snoc (Array.snoc acc l1) l2) rest
    collect acc other = Array.snoc acc other

matchPair :: forall a. Ord a => Lem a -> Lem a -> Kube a -> List Kid
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

matchDict :: forall a. Ord a => Lem a -> Kube a -> List Kid
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

matchDict' :: forall a. Ord a => Dict1 a -> Kube a -> List Kid
matchDict' dict kube = case dict of
  D1 (L1 k) (L1 v) -> matchPair (L1 k) (L1 v) kube
  D2 p1 p2 rest -> matchDict (Dict p1 p2 rest) kube
  _ -> mempty