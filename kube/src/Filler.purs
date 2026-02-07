module Kubrick.Filler  (    fill  )  where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl, foldr, foldMap)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Kubrick.Getter as Getter
import Kubrick.Kube.Types (Kid, Kube)
import Kubrick.Lem (Lem(..), Dict1(..), Sek1(..), Bag1(..))
import Kubrick.Matcher (match)
import Kubrick.Reticolo (Reticolo)
import Kubrick.Reticolo as Reticolo
import Kubrick.Types (Raw, Term(..), Vid, RawType(..))

-- | Internal types for cleaner pattern matching
type Extraction = { vid :: Maybe Vid, values :: Array (Array Raw) }

type ExtractionResult = { vids :: Array Vid, rows :: Array (Array Raw) }
--| given a kube, a Kid and a query Lem Term, return a Reticolo of Vids on which 
--| all the values of the document with that Kid matching with Vids in the query
--| are stored as a single row in the returning Reticolo. ex:
--| kid doc: Sek (L1 (Rs "a")) (L1 (Rs "b")) Nil
--| query: Sek (L1 (Rs "a")) (L1 (TVid 1)) Nil
--| result: Reticolo [Vid 1] with one row [b]
--| all Raw and Gap values in the query are skipped.
--| in case of Bag/Choice, the returned Reticolo will have multiple rows, one for each possible match.
--|   if in the query's Bag/Choice there are values other than Vids, 
--|   those will be excluded from being added in the Reticolo's rows.
--|   if there are multiple Vids in query's Bag/Choice, the returned Reticolo will have multiple columns, 
--|   one for each Vid, and the combinations will be generated accordingly like a cross-product.
--| if the Vid is occupying a nested structure in the kid document, The Vid would not be stored in the Reticolo.
--| if no match return an error

fill :: Kube -> Kid -> Lem Term -> Either String (Reticolo Vid)
fill kube kid query = do
  doc <- getDocument kube kid
  result <- matchAndExtractWithVids query doc
  validateAndBuildReticolo result
  where
    getDocument :: Kube -> Kid -> Either String (Lem Raw)
    getDocument k k_id = case Getter.get k k_id of
      Just d -> Right d
      Nothing -> Left $ "Kid " <> show k_id <> " not found in Kube"
    
    validateAndBuildReticolo :: ExtractionResult -> Either String (Reticolo Vid)
    validateAndBuildReticolo res = 
      if Array.null res.rows
        then Left "No match"
        else buildReticolo res.vids res.rows

-- Extract values and track which Vids have data
matchAndExtractWithVids :: Lem Term -> Lem Raw -> Either String ExtractionResult
matchAndExtractWithVids query doc = 
  extractAllPositions query doc <#> consolidateExtractions

-- Extract from all positions, returning array of extractions
extractAllPositions :: Lem Term -> Lem Raw -> Either String (Array Extraction)
extractAllPositions query doc = case query, doc of
  L1 (TVid v), L1 raw -> Right [{ vid: Just v, values: [[raw]] }]
  L1 (TVid v), _ -> Right [{ vid: Just v, values: [] }]
  L1 (TRaw _), _ -> Right []
  Gap, _ -> Right []
  L0, L0 -> Right []
  L0, _ -> Left "Query L0 does not match document"
  
  Sek qf qs qr, Sek df ds dr -> extractSequence qf qs qr df ds dr
  Bag qf qs qr, Bag df ds dr -> extractChoice qf qs qr df ds dr
  Choice qf qs qr, Choice df ds dr -> extractChoice qf qs qr df ds dr
  Pair qk qv, Pair dk dv -> extractPair qk qv dk dv
  Dict qf qs qr, Dict df ds dr -> extractDict qf qs qr df ds dr
  
  _, _ -> Left "Structure mismatch"

-- Extract from sequence structures (Sek)
extractSequence :: Lem Term -> Lem Term -> List (Lem Term) -> Lem Raw -> Lem Raw -> List (Lem Raw) -> Either String (Array Extraction)
extractSequence qf qs qr df ds dr = do
  when (List.length qr /= List.length dr) $ Left "Sequence length mismatch"
  extractionsFromList [qf, qs] [df, ds] (List.zip qr dr)

-- Extract from unordered structures (Bag/Choice)
extractChoice :: Lem Term -> Lem Term -> List (Lem Term) -> Lem Raw -> Lem Raw -> List (Lem Raw) -> Either String (Array Extraction)
extractChoice qf qs qr df ds dr = 
  extractFromBagChoice (List.Cons qf (List.Cons qs qr)) (List.Cons df (List.Cons ds dr))

-- Extract from key-value pair
extractPair :: Lem Term -> Lem Term -> Lem Raw -> Lem Raw -> Either String (Array Extraction)
extractPair qk qv dk dv = do
  kExt <- extractAllPositions qk dk
  vExt <- extractAllPositions qv dv
  Right $ kExt <> vExt

-- Extract from dictionary structures
extractDict :: Tuple (Lem Term) (Lem Term) -> Tuple (Lem Term) (Lem Term) -> List (Tuple (Lem Term) (Lem Term)) 
           -> Tuple (Lem Raw) (Lem Raw) -> Tuple (Lem Raw) (Lem Raw) -> List (Tuple (Lem Raw) (Lem Raw)) 
           -> Either String (Array Extraction)
extractDict qf qs qr df ds dr = do
  when (List.length qr /= List.length dr) $ Left "Dict length mismatch"
  extractionsFromPairs [qf, qs] [df, ds] (List.zip qr dr)

-- Helper: Extract from list of queries and docs
extractionsFromList :: Array (Lem Term) -> Array (Lem Raw) -> List (Tuple (Lem Term) (Lem Raw)) -> Either String (Array Extraction)
extractionsFromList initial initDocs rest = do
  initExts <- traverse (\(Tuple q d) -> extractAllPositions q d) (Array.zip initial initDocs)
  restExts <- traverse (\(Tuple q d) -> extractAllPositions q d) (Array.fromFoldable rest)
  Right $ Array.fold (initExts <> restExts)

-- Helper: Extract from paired tuples
extractionsFromPairs :: Array (Tuple (Lem Term) (Lem Term)) -> Array (Tuple (Lem Raw) (Lem Raw)) 
                     -> List (Tuple (Tuple (Lem Term) (Lem Term)) (Tuple (Lem Raw) (Lem Raw))) 
                     -> Either String (Array Extraction)
extractionsFromPairs initial initDocs rest = do
  initExts <- traverse extractTuplePair (Array.zip initial initDocs)
  restExts <- traverse extractTuplePair (Array.fromFoldable rest)
  Right $ Array.fold (initExts <> restExts)
  where
    extractTuplePair :: Tuple (Tuple (Lem Term) (Lem Term)) (Tuple (Lem Raw) (Lem Raw)) -> Either String (Array Extraction)
    extractTuplePair (Tuple (Tuple qk qv) (Tuple dk dv)) = do
      kExt <- extractAllPositions qk dk
      vExt <- extractAllPositions qv dv
      Right $ kExt <> vExt

-- For Bag/Choice: Generate all permutations for Vids after filtering TRaw matches
-- Optimized: match and remove TRaw first, then generate permutations only for Vids
extractFromBagChoice :: List (Lem Term) -> List (Lem Raw) -> Either String (Array Extraction)
extractFromBagChoice queryElems docElems = do
  let queryArray = Array.fromFoldable queryElems
      docArray = Array.fromFoldable docElems
      { vids, nonVids } = partitionQueries queryArray
      availableDocs = filterMatchedDocs nonVids docArray
  
  if Array.null vids
    then Right []
    else Right $ generatePermutations vids availableDocs
  where
    partitionQueries :: Array (Lem Term) -> { vids :: Array Vid, nonVids :: Array (Lem Term) }
    partitionQueries qs = 
      let vids = Array.sort $ Array.mapMaybe extractVid qs  -- Sort vids for deterministic order
          nonVids = Array.filter isNonVid qs
      in { vids, nonVids }
    
    extractVid :: Lem Term -> Maybe Vid
    extractVid (L1 (TVid v)) = Just v
    extractVid _ = Nothing
    
    isNonVid :: Lem Term -> Boolean
    isNonVid (L1 (TVid _)) = false
    isNonVid (L1 (TRaw _)) = true
    isNonVid _ = false
    
    filterMatchedDocs :: Array (Lem Term) -> Array (Lem Raw) -> Array (Lem Raw)
    filterMatchedDocs nonVids docs =
      let matched = Array.mapMaybe (matchNonVid docs) nonVids
      in Array.filter (\d -> not (Array.elem d matched)) docs
    
    matchNonVid :: Array (Lem Raw) -> Lem Term -> Maybe (Lem Raw)
    matchNonVid docs (L1 (TRaw r)) = 
      Array.find (\d -> case d of
        L1 dr -> dr == r
        _ -> false) docs
    matchNonVid _ _ = Nothing
    
    -- Generate all permutations: each Vid gets assigned values from available docs
    -- Returns one Extraction per Vid position with all possible values across permutations
    -- Sort docs first for deterministic permutation order
    generatePermutations :: Array Vid -> Array (Lem Raw) -> Array Extraction
    generatePermutations vids docs = 
      let rawDocs = Array.sort $ Array.mapMaybe extractRawValue docs
          allAssignments = generateAssignments vids rawDocs
      in transposeAssignments vids allAssignments
    
    -- Generate all ways to assign N docs to N Vid positions (permutations)
    generateAssignments :: Array Vid -> Array Raw -> Array (Array (Maybe Raw))
    generateAssignments vids docs = case Array.uncons vids of
      Nothing -> [[]]
      Just { head: _, tail: [] } ->
        -- Single Vid: each doc becomes one assignment
        map (\d -> [Just d]) docs
      Just { head: _, tail: restVids } -> do
        -- Multi-Vid: try assigning each doc to first position
        doc <- docs
        let remainingDocs = Array.filter (\d -> d /= doc) docs
            restAssignments = generateAssignments restVids remainingDocs
        assignment <- restAssignments
        pure $ Array.cons (Just doc) assignment
    
    -- Transpose assignments into per-Vid format
    transposeAssignments :: Array Vid -> Array (Array (Maybe Raw)) -> Array Extraction
    transposeAssignments vids assignments =
      Array.mapWithIndex (\idx vid ->
        let values = Array.mapMaybe (\assignment ->
              case Array.index assignment idx of
                Just (Just v) -> Just [v]
                _ -> Nothing
            ) assignments
        in { vid: Just vid, values }
      ) vids
    
    extractRawValue :: Lem Raw -> Maybe Raw
    extractRawValue (L1 r) = Just r
    extractRawValue _ = Nothing

-- Consolidate extractions: build Vids array and align rows
consolidateExtractions :: Array Extraction -> ExtractionResult
consolidateExtractions extractions =
  let withVids = filterValidExtractions extractions
      vids = Array.mapMaybe _.vid withVids
      rowArrays = map _.values withVids
      rows = alignOrCombineRows rowArrays
  in { vids, rows }
  where
    filterValidExtractions :: Array Extraction -> Array Extraction
    filterValidExtractions = Array.filter \e -> case e.vid of
      Just _ -> not (Array.null e.values)
      Nothing -> false
    
    alignOrCombineRows :: Array (Array (Array Raw)) -> Array (Array Raw)
    alignOrCombineRows arr = 
      if isAligned arr then zipRows arr else combineRows arr
    
    isAligned :: Array (Array (Array Raw)) -> Boolean
    isAligned arr = case Array.uncons arr of
      Nothing -> true
      Just { head, tail } -> Array.all (\a -> Array.length a == Array.length head) tail
    
    zipRows :: Array (Array (Array Raw)) -> Array (Array Raw)
    zipRows arr = case Array.uncons arr of
      Nothing -> []
      Just { head: first, tail } ->
        Array.mapWithIndex (\i row ->
          let restValues = Array.mapMaybe (\rows -> Array.index rows i) tail
          in foldl append row restValues
        ) first

-- Extract all Vid headers from a query in order (only TVids)
extractVids :: Lem Term -> Array Vid
extractVids = case _ of
  L1 (TVid v) -> [v]
  L1 (TRaw _) -> []
  Gap -> []
  L0 -> []
  Pair l r -> extractVids l <> extractVids r
  Sek f s r -> extractFromTriple f s r
  Bag f s r -> extractFromTriple f s r
  Choice f s r -> extractFromTriple f s r
  Dict f s r -> extractFromDictTriple f s r
  Sekdict _ _ -> []
  Bagdict _ _ -> []
  where
    extractFromTriple :: Lem Term -> Lem Term -> List (Lem Term) -> Array Vid
    extractFromTriple f s r = 
      extractVids f <> extractVids s <> foldMap extractVids r
    
    extractFromDictTriple :: Tuple (Lem Term) (Lem Term) -> Tuple (Lem Term) (Lem Term) -> List (Tuple (Lem Term) (Lem Term)) -> Array Vid
    extractFromDictTriple f s r =
      extractTuple f <> extractTuple s <> foldMap extractTuple r
    
    extractTuple :: Tuple (Lem Term) (Lem Term) -> Array Vid
    extractTuple (Tuple k v) = extractVids k <> extractVids v

-- Build Reticolo from Vid headers and rows of Raw values
buildReticolo :: Array Vid -> Array (Array Raw) -> Either String (Reticolo Vid)
buildReticolo vids rows = do
  retWithCols <- foldr addColumn (Right Reticolo.empty) vids
  foldl addRow (Right retWithCols) rows
  where
    addColumn :: Vid -> Either String (Reticolo Vid) -> Either String (Reticolo Vid)
    addColumn vid eitherRet = eitherRet >>= Reticolo.addColumn vid TString []
    
    addRow :: Either String (Reticolo Vid) -> Array Raw -> Either String (Reticolo Vid)
    addRow eitherRet row = eitherRet >>= \ret -> Reticolo.addRow row ret

-- Combine rows from multiple positions using cross product
combineRows :: Array (Array (Array Raw)) -> Array (Array Raw)
combineRows = Array.foldr combineStep [[]]
  where
    combineStep :: Array (Array Raw) -> Array (Array Raw) -> Array (Array Raw)
    combineStep values acc
      | Array.null values = acc
      | otherwise = do
          row <- values
          rest <- acc
          pure $ row <> rest
