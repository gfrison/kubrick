module Reticolo 
  ( Reticolo
  , module Kubrick.Types
  , Column
  , empty
  , addColumn
  , removeColumn
  , addRow
  , derivedColumn
  , aggregate
  , AggregateOp(..)
  , innerJoin
  , cartesian
  , union
  , getColumn
  , getRow
  , getMap
  , rowCount
  , columnCount
  ) where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Map as Map
import Data.Set as Set
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..), either)
import Data.Foldable (foldl)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Kubrick.Types (Raw(..), RawType(..))

-- | Get the type of a cell value
cellType :: Raw -> RawType
cellType (Ri _) = TInt
cellType (Rf _) = TFloat
cellType (Rs _) = TString
cellType (Rb _) = TBool

-- | A column with typed header and homogeneous values
type Column h =
  { header :: h
  , colType :: RawType
  , values :: Array Raw
  }

-- | The main 2D table structure with typed headers
newtype Reticolo h = Reticolo
  { columns :: Array (Column h)
  }

derive instance eqReticolo :: Eq h => Eq (Reticolo h)

instance showReticolo :: Show h => Show (Reticolo h) where
  show (Reticolo { columns }) = "Reticolo { columns: " <> show (map _.header columns) <> " }"

-- | Create an empty Reticolo
empty :: forall h. Reticolo h
empty = Reticolo { columns: [] }

-- | Get the number of rows (based on first column)
rowCount :: forall h. Reticolo h -> Int
rowCount (Reticolo { columns }) =
  case Array.head columns of
    Nothing -> 0
    Just col -> Array.length col.values

-- | Get the number of columns
columnCount :: forall h. Reticolo h -> Int
columnCount (Reticolo { columns }) = Array.length columns

-- | Add a new column to the Reticolo
-- | Returns Left with error message if column length doesn't match existing rows or types don't match
addColumn :: forall h. Eq h => h -> RawType -> Array Raw -> Reticolo h -> Either String (Reticolo h)
addColumn header colType values (Reticolo ret) =
  let
    currentRows = rowCount (Reticolo ret)
    newRows = Array.length values
  in
    if currentRows /= 0 && currentRows /= newRows then
      Left $ "Column length mismatch: expected " <> show currentRows <> " but got " <> show newRows
    else if not (Array.all (\v -> cellType v == colType) values) then
      Left $ "Column type mismatch: expected " <> show colType <> " but got mixed types"
    else
      Right $ Reticolo { columns: ret.columns <> [{ header, colType, values }] }

-- | Remove a column by header
removeColumn :: forall h. Eq h => h -> Reticolo h -> Reticolo h
removeColumn header (Reticolo ret) =
  Reticolo { columns: Array.filter (\col -> col.header /= header) ret.columns }

-- | Add a row to the Reticolo
-- | Values must match column order and types
-- | Returns Left with error message if row length or types don't match
addRow :: forall h. Array Raw -> Reticolo h -> Either String (Reticolo h)
addRow values (Reticolo ret) =
  if Array.length values /= Array.length ret.columns then
    Left $ "Row length mismatch: expected " <> show (Array.length ret.columns) <> " values"
  else
    let
      -- Check types match
      typesMatch = Array.zipWith (\col val -> cellType val == col.colType) ret.columns values
                   # Array.all identity
    in
      if not typesMatch then
        Left "Row value types don't match column types"
      else
        let
          newColumns = Array.zipWith (\col val -> col { values = Array.snoc col.values val }) ret.columns values
        in
          Right $ Reticolo { columns: newColumns }

-- | Get a column by header
getColumn :: forall h. Eq h => h -> Reticolo h -> Maybe (Column h)
getColumn header (Reticolo { columns }) =
  Array.find (\col -> col.header == header) columns

-- | Get a row by index
getRow :: forall h. Int -> Reticolo h -> Maybe (Array Raw)
getRow idx (Reticolo { columns }) =
  traverse (\col -> Array.index col.values idx) columns

-- | Get a row by index as a Map from header to value
getMap :: forall h. Ord h => Int -> Reticolo h -> Maybe (Map.Map h Raw)
getMap idx (Reticolo { columns }) = do
  values <- traverse (\col -> Array.index col.values idx) columns
  let headers = map _.header columns
  pure $ Map.fromFoldable $ Array.zip headers values

-- | Create a derived column from two columns using a binary operation
-- | Returns Left with error message if source columns not found or lengths don't match
derivedColumn :: forall h. Eq h => Show h => 
  h -> h -> h -> (Raw -> Raw -> Raw) -> Reticolo h -> Either String (Reticolo h)
derivedColumn sourceHeader1 sourceHeader2 targetHeader op ret =
  case Tuple (getColumn sourceHeader1 ret) (getColumn sourceHeader2 ret) of
    Tuple (Just col1) (Just col2) ->
      if Array.length col1.values /= Array.length col2.values then
        Left "Column lengths don't match for derived column"
      else
        let
          newValues = Array.zipWith op col1.values col2.values
          newType = case Array.head newValues of
            Nothing -> TInt  -- Default type for empty columns
            Just v -> cellType v
        in
          addColumn targetHeader newType newValues ret
    _ ->
      Left $ "Source columns not found: " <> show sourceHeader1 <> ", " <> show sourceHeader2

-- | Aggregation operations
data AggregateOp
  = Sum
  | Count
  | Avg
  | Min
  | Max

derive instance eqAggregateOp :: Eq AggregateOp

-- | Aggregate a column and create a new column with the result
-- | The aggregated value is placed in every row of the new column
-- | Returns Left with error message if source column not found
aggregate :: forall h. Eq h => Show h => h -> h -> AggregateOp -> Reticolo h -> Either String (Reticolo h)
aggregate sourceHeader targetHeader op ret =
  case getColumn sourceHeader ret of
    Nothing -> Left $ "Source column not found: " <> show sourceHeader
    Just col ->
      let
        result = computeAggregate op col.values
        rows = rowCount ret
        newValues = Array.replicate rows result
        newType = cellType result
      in
        addColumn targetHeader newType newValues ret

-- | Compute aggregate value
computeAggregate :: AggregateOp -> Array Raw -> Raw
computeAggregate Sum values =
  Rf $ foldl (\acc v -> acc + (either (const 0.0) identity $ toNumber v)) 0.0 values
computeAggregate Count values =
  Ri $ Array.length values
computeAggregate Avg values =
  let total = foldl (\acc v -> acc + (either (const 0.0) identity $ toNumber v)) 0.0 values
      count = Array.length values
  in Rf $ if count == 0 then 0.0 else total / (Int.toNumber count)
computeAggregate Min values =
  fromMaybe (Ri 0) (Array.head $ Array.sort values)
computeAggregate Max values =
  fromMaybe (Ri 0) (Array.last $ Array.sort values)

-- | Convert cell value to number (for aggregation)
-- | Returns Left with error message if value is a String
toNumber :: Raw -> Either String Number
toNumber (Ri n) = Right $ Int.toNumber n
toNumber (Rf n) = Right n
toNumber (Rs _) = Left "Cannot convert string to number"
toNumber (Rb b) = Right $ if b then 1.0 else 0.0

-- ============================================================================
-- Helper functions for table operations
-- ============================================================================

-- | Get all headers from a Reticolo
getHeaders :: forall h. Reticolo h -> Array h
getHeaders (Reticolo { columns }) = map _.header columns

-- | Check if two Reticolo tables have identical headers (ignoring order)
hasIdenticalHeaders :: forall h. Eq h => Ord h => Reticolo h -> Reticolo h -> Boolean
hasIdenticalHeaders retA retB =
  Array.sort (getHeaders retA) == Array.sort (getHeaders retB)

-- | Find common headers between two tables
commonHeaders :: forall h. Eq h => Reticolo h -> Reticolo h -> Array h
commonHeaders retA retB =
  let
    headersA = getHeaders retA
    headersB = getHeaders retB
  in
    Array.filter (\h -> Array.elem h headersB) headersA

-- | Filter columns by header membership
filterColumnsByHeaders :: forall h. Eq h => Boolean -> Array h -> Array (Column h) -> Array (Column h)
filterColumnsByHeaders include headers columns =
  Array.filter (\col -> Array.elem col.header headers == include) columns

-- | Get all rows from a Reticolo as an array of arrays
getAllRows :: forall h. Reticolo h -> Array (Array Raw)
getAllRows ret =
  let count = rowCount ret
  in Array.mapMaybe (\idx -> getRow idx ret) (Array.range 0 (count - 1))

-- | Build columns from an array of rows
buildColumnsFromRows :: forall h. Array (Column h) -> Array (Array Raw) -> Array (Column h)
buildColumnsFromRows templateColumns rows =
  Array.mapWithIndex (\colIdx col ->
    col { values = Array.mapMaybe (\row -> Array.index row colIdx) rows }
  ) templateColumns

-- ============================================================================
-- Join and set operations
-- ============================================================================

-- | Inner join two Reticolo tables on shared headers
-- | Uses sort-merge join algorithm for efficiency
-- | Only rows where ALL shared column values match are included
-- | Shared columns appear once in the result
-- | Returns empty Reticolo if no matches found
innerJoin :: forall h. Eq h => Ord h => Reticolo h -> Reticolo h -> Reticolo h
innerJoin retLeft@(Reticolo left) retRight@(Reticolo right) =
  let
    common = commonHeaders retLeft retRight
  in
    -- If no common headers, return empty
    if Array.null common
      then Reticolo { columns: [] }
      else
        let
          -- Partition columns into common and unique
          leftOnlyColumns = filterColumnsByHeaders false common left.columns
          rightOnlyColumns = filterColumnsByHeaders false common right.columns
          commonColumns = filterColumnsByHeaders true common left.columns
          
          -- Extract and sort join keys
          leftKeys = buildIndexedKeys common retLeft
          rightKeys = buildIndexedKeys common retRight
          sortedLeft = Array.sortBy (\a b -> compare a.key b.key) leftKeys
          sortedRight = Array.sortBy (\a b -> compare a.key b.key) rightKeys
          
          -- Perform sort-merge join
          matchedPairs = sortMergeJoin sortedLeft sortedRight
          
          -- Build result columns from matched pairs
          resultCommonColumns = buildColumnsFromPairs commonColumns matchedPairs (\(Tuple l _) -> l)
          resultLeftOnlyColumns = buildColumnsFromPairs leftOnlyColumns matchedPairs (\(Tuple l _) -> l)
          resultRightOnlyColumns = buildColumnsFromPairs rightOnlyColumns matchedPairs (\(Tuple _ r) -> r)
        in
          Reticolo { columns: resultCommonColumns <> resultLeftOnlyColumns <> resultRightOnlyColumns }

-- | Build indexed keys for join
buildIndexedKeys :: forall h. Eq h => Array h -> Reticolo h -> Array { idx :: Int, key :: Array Raw }
buildIndexedKeys headers ret =
  Array.mapWithIndex (\idx _ -> 
    { idx, key: extractKey idx headers ret }
  ) (Array.range 0 (rowCount ret - 1))

-- | Build columns from row index pairs
buildColumnsFromPairs :: forall h. Array (Column h) -> Array (Tuple Int Int) -> (Tuple Int Int -> Int) -> Array (Column h)
buildColumnsFromPairs columns pairs extractIdx =
  map (\col ->
    col { values = Array.mapMaybe (\pair -> Array.index col.values (extractIdx pair)) pairs }
  ) columns

-- | Extract join key values for a row
extractKey :: forall h. Eq h => Int -> Array h -> Reticolo h -> Array Raw
extractKey idx headers (Reticolo ret) =
  Array.mapMaybe (\header ->
    case Array.find (\col -> col.header == header) ret.columns of
      Just col -> Array.index col.values idx
      Nothing -> Nothing
  ) headers

-- | Sort-merge join algorithm
-- | Merges two sorted lists of indexed keys
sortMergeJoin :: Array { idx :: Int, key :: Array Raw } -> Array { idx :: Int, key :: Array Raw } -> Array (Tuple Int Int)
sortMergeJoin leftSorted rightSorted = go 0 0 []
  where
    go :: Int -> Int -> Array (Tuple Int Int) -> Array (Tuple Int Int)
    go i j acc
      | i >= Array.length leftSorted = acc
      | j >= Array.length rightSorted = acc
      | otherwise =
          case Tuple (Array.index leftSorted i) (Array.index rightSorted j) of
            Tuple (Just l) (Just r) ->
              case compare l.key r.key of
                LT -> go (i + 1) j acc  -- Left key smaller, advance left
                GT -> go i (j + 1) acc  -- Right key smaller, advance right
                EQ ->
                  -- Keys match - collect all matching pairs
                  let
                    -- Find all left rows with same key
                    leftMatches = Array.takeWhile (\x -> x.key == l.key) (Array.drop i leftSorted)
                    -- Find all right rows with same key
                    rightMatches = Array.takeWhile (\x -> x.key == r.key) (Array.drop j rightSorted)
                    -- Cartesian product of matching rows
                    pairs = Array.concatMap (\lm ->
                      map (\rm -> Tuple lm.idx rm.idx) rightMatches
                    ) leftMatches
                  in
                    go (i + Array.length leftMatches) (j + Array.length rightMatches) (acc <> pairs)
            _ -> acc

-- | Cartesian product of two Reticolo tables
-- | Each row from the left table is combined with each row from the right table
cartesian :: forall h. Eq h => Reticolo h -> Reticolo h -> Reticolo h
cartesian (Reticolo left) (Reticolo right) =
  let
    leftRows = rowCount (Reticolo left)
    rightRows = rowCount (Reticolo right)
    
    -- For each column, replicate values according to cartesian product
    expandedLeft = map (\col -> 
      col { values = Array.concatMap (\v -> Array.replicate rightRows v) col.values }
    ) left.columns
    
    expandedRight = map (\col ->
      col { values = Array.concat $ Array.replicate leftRows col.values }
    ) right.columns
  in
    Reticolo { columns: expandedLeft <> expandedRight }

-- | Union of two Reticolo tables with identical headers
-- | Combines all rows from both tables, deduplicating identical rows
-- | Returns empty if headers don't match
union :: forall h. Eq h => Ord h => Reticolo h -> Reticolo h -> Reticolo h
union retA@(Reticolo left) retB =
  -- Verify identical headers
  if not (hasIdenticalHeaders retA retB)
    then Reticolo { columns: [] }
    else
      let
        -- Extract all rows from both tables
        leftRows = getAllRows retA
        rightRows = getAllRows retB
        
        -- Combine and deduplicate using Set
        allRows = leftRows <> rightRows
        uniqueRows = Set.toUnfoldable $ Set.fromFoldable allRows :: Array (Array Raw)
        
        -- Rebuild columns from unique rows
        resultColumns = buildColumnsFromRows left.columns uniqueRows
      in
        Reticolo { columns: resultColumns }
