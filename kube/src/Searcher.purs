module Kubrick.Searcher where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl)
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

search :: Kube -> Lem Term -> Reticolo Vid
search kube _query = 
  let 
    nlem :: Lem Raw
    nlem = termToRaw _query
    _ = match kube nlem
  in
    Reticolo.empty
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
  doc <- case Getter.get kube kid of
    Just d -> Right d
    Nothing -> Left $ "Kid " <> show kid <> " not found in Kube"
  
  -- Extract rows with Vid tracking
  result <- matchAndExtractWithVids query doc
  
  if Array.null result.rows
    then Left "No match"
    else
      buildReticolo result.vids result.rows
      
-- Extract values and track which Vids have data
-- Returns: { vids :: Array Vid, rows :: Array (Array Raw) }
matchAndExtractWithVids :: Lem Term -> Lem Raw -> Either String { vids :: Array Vid, rows :: Array (Array Raw) }
matchAndExtractWithVids query doc = do
  extracted <- extractAllPositions query doc
  Right $ consolidateExtractions extracted

-- Extract from all positions, returning array of { vid :: Maybe Vid, values :: Array (Array Raw) }
extractAllPositions :: Lem Term -> Lem Raw -> Either String (Array { vid :: Maybe Vid, values :: Array (Array Raw) })
extractAllPositions query doc = case query, doc of
  -- L1 cases  
  L1 (TVid v), L1 raw -> Right [{ vid: Just v, values: [[raw]] }]
  L1 (TVid v), _ -> Right [{ vid: Just v, values: [] }]  -- Nested, omit
  L1 (TRaw _), _ -> Right []  -- Skip like Gap
  
  -- Gap - skip
  Gap, _ -> Right []
  
  -- L0
  L0, L0 -> Right []
  L0, _ -> Left "Query L0 does not match document"
  
  -- Sek - extract from each position
  Sek qf qs qr, Sek df ds dr -> do
    when (List.length qr /= List.length dr) $ 
      Left "Sek length mismatch"
    fExt <- extractAllPositions qf df
    sExt <- extractAllPositions qs ds
    restExt <- traverse (\(Tuple q d) -> extractAllPositions q d) (List.zip qr dr)
    Right $ fExt <> sExt <> Array.foldMap identity (Array.fromFoldable restExt)
  
  -- Bag/Choice - try all matchings
  Bag qf qs qr, Bag df ds dr -> 
    extractFromBagChoice (List.Cons qf (List.Cons qs qr)) (List.Cons df (List.Cons ds dr))
  Choice qf qs qr, Choice df ds dr -> 
    extractFromBagChoice (List.Cons qf (List.Cons qs qr)) (List.Cons df (List.Cons ds dr))
  
  -- Pair
  Pair qk qv, Pair dk dv -> do
    kExt <- extractAllPositions qk dk
    vExt <- extractAllPositions qv dv
    Right $ kExt <> vExt
  
  -- Dict - extract from all key-value pairs
  Dict qf qs qr, Dict df ds dr -> do
    when (List.length qr /= List.length dr) $ 
      Left "Dict length mismatch"
    fExt <- extractFromTuplePair qf df
    sExt <- extractFromTuplePair qs ds
    restExt <- traverse (\(Tuple q d) -> extractFromTuplePair q d) (List.zip qr dr)
    Right $ fExt <> sExt <> Array.foldMap identity (Array.fromFoldable restExt)
  
  _, _ -> Left "Structure mismatch"
  where
    extractFromTuplePair :: Tuple (Lem Term) (Lem Term) -> Tuple (Lem Raw) (Lem Raw) -> Either String (Array { vid :: Maybe Vid, values :: Array (Array Raw) })
    extractFromTuplePair (Tuple qk qv) (Tuple dk dv) = do
      kExt <- extractAllPositions qk dk
      vExt <- extractAllPositions qv dv
      Right $ kExt <> vExt

-- For Bag/Choice: Each query element can match each doc element
-- Generate all valid assignments where each doc element is used at most once per row
-- TRaw elements match doc elements and exclude them from TVid matching
extractFromBagChoice :: List (Lem Term) -> List (Lem Raw) -> Either String (Array { vid :: Maybe Vid, values :: Array (Array Raw) })
extractFromBagChoice queryElems docElems = do
  -- Separate Vid and non-Vid query elements
  let queryArray = Array.fromFoldable queryElems
      vidQueries = Array.mapMaybe (\q -> case q of
                                      L1 (TVid v) -> Just v
                                      _ -> Nothing) queryArray
      nonVidQueries = Array.filter (\q -> case q of
                                      L1 (TVid _) -> false
                                      L1 (TRaw _) -> true
                                      _ -> false) queryArray
      docArray = Array.fromFoldable docElems
  
  -- Match non-Vid queries first and remove matched docs
  let matchedByNonVid = Array.mapMaybe matchNonVid nonVidQueries
      availableDocs = Array.filter (\d -> not (Array.elem d matchedByNonVid)) docArray
  
  -- If no Vids, return empty
  if Array.null vidQueries
    then Right []
    else do
      -- Generate permutations only with available docs
      let matchings = generateMatchings vidQueries availableDocs
      Right $ transposeMatchings vidQueries matchings
  where
    -- Try to find a match for a non-Vid query element
    matchNonVid :: Lem Term -> Maybe (Lem Raw)
    matchNonVid (L1 (TRaw r)) = 
      -- Find first doc that matches this Raw value
      Array.find (\d -> case d of
                          L1 dr -> dr == r
                          _ -> false) (Array.fromFoldable docElems)
    matchNonVid _ = Nothing
    
    -- Generate all ways to assign doc elements to query Vid positions
    -- Returns: Array of matchings, where each matching is Array of values (one per Vid)
    generateMatchings :: Array Vid -> Array (Lem Raw) -> Array (Array { vid :: Vid, value :: Maybe Raw })
    generateMatchings vids docs = case Array.uncons vids of
      Nothing -> []
      Just { head: onlyVid, tail: [] } ->
        -- Single Vid: each doc becomes a matching
        map (\d -> [{ vid: onlyVid, value: extractRaw d }]) docs
      Just { head: firstVid, tail: restVids } -> do
        -- Multi-Vid case: try assigning each doc to first Vid position
        doc <- docs
        let remainingDocs = Array.filter (\d -> d /= doc) docs
            firstAssignment = { vid: firstVid, value: extractRaw doc }
            restMatchings = generateMatchings restVids remainingDocs
        matching <- restMatchings
        pure $ Array.cons firstAssignment matching
    
    extractRaw :: Lem Raw -> Maybe Raw
    extractRaw (L1 r) = Just r
    extractRaw _ = Nothing
    
    -- Transpose: convert matchings (rows) to per-position format
    -- Each matching becomes one row in the final result
    transposeMatchings :: Array Vid -> Array (Array { vid :: Vid, value :: Maybe Raw }) -> Array { vid :: Maybe Vid, values :: Array (Array Raw) }
    transposeMatchings vids matchings =
      Array.mapWithIndex (\pos vid ->
        let values = Array.mapMaybe (\matching ->
              case Array.index matching pos of
                Just entry -> case entry.value of
                  Just v -> Just [v]
                  Nothing -> Nothing
                Nothing -> Nothing
            ) matchings
        in { vid: Just vid, values }
      ) vids

--Consolidate extractions: build Vids array and align rows
consolidateExtractions :: Array { vid :: Maybe Vid, values :: Array (Array Raw) } -> { vids :: Array Vid, rows :: Array (Array Raw) }
consolidateExtractions extractions =
  let withVids = Array.filter (\e -> case e.vid of
                                        Just _ -> not (Array.null e.values)
                                        Nothing -> false) extractions
      vids = Array.mapMaybe _.vid withVids
      rowArrays = map _.values withVids
      -- Check if all positions have the same number of rows (aligned) or different (cross-product)
      rows = if isAligned rowArrays
               then zipRows rowArrays
               else combineRows rowArrays
  in { vids, rows }
  where
    -- Check if all row arrays have the same length (means they're aligned, not independent)
    isAligned :: Array (Array (Array Raw)) -> Boolean
    isAligned arr = case Array.uncons arr of
      Nothing -> true
      Just { head, tail } ->
        let len = Array.length head
        in Array.all (\a -> Array.length a == len) tail
    
    -- Zip aligned rows (for Bag where rows are correlated)
    zipRows :: Array (Array (Array Raw)) -> Array (Array Raw)
    zipRows arr = case Array.uncons arr of
      Nothing -> []
      Just { head: first, tail } ->
        Array.mapWithIndex (\i row ->
          let restValues = Array.mapMaybe (\rows -> Array.index rows i) tail
              allValues = Array.foldl (\acc r -> acc <> r) row restValues
          in allValues
        ) first

-- Extract all Vid headers from a query in order (only TVids)
extractVids :: Lem Term -> Array Vid
extractVids = case _ of
  L1 (TVid v) -> [v]
  L1 (TRaw _) -> []
  Gap -> []
  L0 -> []
  Pair l r -> extractVids l <> extractVids r
  Sek f s r -> extractVids f <> extractVids s <> Array.foldMap extractVids (Array.fromFoldable r)
  Bag f s r -> extractVids f <> extractVids s <> Array.foldMap extractVids (Array.fromFoldable r)
  Choice f s r -> extractVids f <> extractVids s <> Array.foldMap extractVids (Array.fromFoldable r)
  Dict f s r -> 
    let extractTuple (Tuple k v) = extractVids k <> extractVids v
    in extractTuple f <> extractTuple s <> Array.foldMap extractTuple (Array.fromFoldable r)
  Sekdict _ _ -> []  -- TODO: handle if needed
  Bagdict _ _ -> []  -- TODO: handle if needed

-- Build Reticolo from Vid headers and rows of Raw values
buildReticolo :: Array Vid -> Array (Array Raw) -> Either String (Reticolo Vid)
buildReticolo vids rows = do
  -- Start with empty Reticolo
  let initial = Reticolo.empty
  -- Add columns for each Vid
  retWithCols <- foldl addCol (Right initial) vids
  -- Add rows
  foldl addR (Right retWithCols) rows
  where
    addCol :: Either String (Reticolo Vid) -> Vid -> Either String (Reticolo Vid)
    addCol (Left err) _ = Left err
    addCol (Right ret) vid = Reticolo.addColumn vid TString [] ret
    
    addR :: Either String (Reticolo Vid) -> Array Raw -> Either String (Reticolo Vid)
    addR (Left err) _ = Left err
    addR (Right ret) row = Reticolo.addRow row ret

-- Combine rows from multiple positions, filtering out empty positions
combineRows :: Array (Array (Array Raw)) -> Array (Array Raw)
combineRows arr = case Array.uncons arr of
  Nothing -> [[]]
  Just { head: x, tail: [] } -> x
  Just { head: x, tail: xs } -> 
    -- Skip empty positions
    if Array.null x then combineRows xs
    else do
      row <- x
      rest <- combineRows xs
      pure $ row <> rest

termToRaw :: Lem Term -> Lem Raw
termToRaw = case _ of
  L1 (TVid _) -> Gap
  L1 (TRaw r) -> L1 r
  Gap -> Gap
  L0 -> L0
  
  Sek f s r -> Sek (termToRaw f) (termToRaw s) (map termToRaw r)
  Bag f s r -> Bag (termToRaw f) (termToRaw s) (map termToRaw r)
  Choice f s r -> Choice (termToRaw f) (termToRaw s) (map termToRaw r)
  
  Pair k v -> Pair (termToRaw k) (termToRaw v)
  
  Dict fst snd rest -> Dict (mapPair fst) (mapPair snd) (map mapPair rest)
    where mapPair (Tuple k v) = Tuple (termToRaw k) (termToRaw v)
  
  Sekdict sek dict -> Sekdict (sek1ToRaw sek) (dict1ToRaw dict)
  Bagdict bag dict -> Bagdict (bag1ToRaw bag) (dict1ToRaw dict)
sek1ToRaw :: Sek1 Term -> Sek1 Raw
sek1ToRaw input = case input of
  S1 t -> S1 (termToRaw t)
  S2 t1 t2 rest -> S2 (termToRaw t1) (termToRaw t2) (map termToRaw rest)
bag1ToRaw :: Bag1 Term -> Bag1 Raw
bag1ToRaw input = case input of
  B1 t -> B1 (termToRaw t)
  B2 t1 t2 rest -> B2 (termToRaw t1) (termToRaw t2) (map termToRaw rest)
dict1ToRaw :: Dict1 Term -> Dict1 Raw
dict1ToRaw input = case input of
  D1 k v  -> D1 (termToRaw k) (termToRaw v)
  D2 (Tuple k1 v1) (Tuple k2 v2) rest -> D2 (Tuple (termToRaw k1) (termToRaw v1)) (Tuple (termToRaw k2) (termToRaw v2)) (map mapPair rest)
    where mapPair (Tuple k v) = Tuple (termToRaw k) (termToRaw v)