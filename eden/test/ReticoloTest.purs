module Test.ReticoloTest where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), isNothing)
import Partial.Unsafe (unsafeCrashWith)
import Reticolo as R
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

-- | Helper to unwrap Either results in tests - assumes Right
-- | Tests should fail if Left is encountered
unsafeFromRight :: forall a b. Either a b -> b
unsafeFromRight (Right b) = b
unsafeFromRight (Left _) = unsafeCrashWith "Expected Right, got Left"

-- | Helper for chaining operations that return Either in tests
-- | Unwraps the Either and passes the value to the next function
infixl 1 bindUnsafe as ##

bindUnsafe :: forall a b. Either String a -> (a -> Either String b) -> b
bindUnsafe ea f = unsafeFromRight (ea >>= f)

spec :: Spec Unit
spec = do
  describe "Reticolo - Basic Operations" do
    
    it "creates an empty Reticolo" do
      let ret = (R.empty :: R.Reticolo String)
      R.rowCount ret `shouldEqual` 0
      R.columnCount ret `shouldEqual` 0
    
    it "adds a single column to empty Reticolo" do
      let ret = R.addColumn "A" R.TInt [R.Ri 1, R.Ri 2, R.Ri 3] (R.empty :: R.Reticolo String)
                # unsafeFromRight
      R.rowCount ret `shouldEqual` 3
      R.columnCount ret `shouldEqual` 1
    
    it "adds multiple columns" do
      let ret = R.addColumn "A" R.TInt [R.Ri 1, R.Ri 2] (R.empty :: R.Reticolo String)
                >>= R.addColumn "B" R.TString [R.Rs "x", R.Rs "y"]
                >>= R.addColumn "C" R.TFloat [R.Rf 1.5, R.Rf 2.5]
                # unsafeFromRight
      R.rowCount ret `shouldEqual` 2
      R.columnCount ret `shouldEqual` 3
    
    it "removes a column by header" do
      let ret = R.addColumn "A" R.TInt [R.Ri 1] (R.empty :: R.Reticolo String)
                >>= R.addColumn "B" R.TInt [R.Ri 2]
                >>= R.addColumn "C" R.TInt [R.Ri 3]
                # unsafeFromRight
                # R.removeColumn "B"
      R.columnCount ret `shouldEqual` 2
      isNothing (R.getColumn "B" ret) `shouldEqual` true
    
    it "adds a row with matching types" do
      let ret = R.addColumn "A" R.TInt [R.Ri 1] (R.empty :: R.Reticolo String)
                >>= R.addColumn "B" R.TString [R.Rs "x"]
                >>= R.addRow [R.Ri 2, R.Rs "y"]
                # unsafeFromRight
      R.rowCount ret `shouldEqual` 2
    
    it "adds multiple rows" do
      let ret = R.addColumn "A" R.TInt [] (R.empty :: R.Reticolo String)
                >>= R.addRow [R.Ri 1]
                >>= R.addRow [R.Ri 2]
                >>= R.addRow [R.Ri 3]
                # unsafeFromRight
      R.rowCount ret `shouldEqual` 3

  describe "Reticolo - Type Validation" do
    
    it "validates column type consistency" do
      let ret = R.addColumn "A" R.TInt [R.Ri 1, R.Ri 2, R.Ri 3] (R.empty :: R.Reticolo String)
                # unsafeFromRight
      case R.getColumn "A" ret of
        Nothing -> false `shouldEqual` true
        Just col -> col.colType `shouldEqual` R.TInt
    
    it "validates different cell types" do
      let ret1 = R.addColumn "Int" R.TInt [R.Ri 42] (R.empty :: R.Reticolo String)
                 # unsafeFromRight
      let ret2 = ret1 # Right >>= R.addColumn "Float" R.TFloat [R.Rf 3.14] # unsafeFromRight
      let ret3 = ret2 # Right >>= R.addColumn "String" R.TString [R.Rs "hello"] # unsafeFromRight
      let ret4 = ret3 # Right >>= R.addColumn "Bool" R.TBool [R.Rb true] # unsafeFromRight
      R.columnCount ret4 `shouldEqual` 4

  describe "Reticolo - Column Operations" do
    
    it "gets column by header" do
      let ret = R.addColumn "X" R.TInt [R.Ri 10, R.Ri 20] (R.empty :: R.Reticolo String)
                # unsafeFromRight
      case R.getColumn "X" ret of
        Nothing -> false `shouldEqual` true
        Just col -> do
          col.header `shouldEqual` "X"
          col.colType `shouldEqual` R.TInt
          Array.length col.values `shouldEqual` 2
    
    it "returns Nothing for non-existent column" do
      let ret = R.addColumn "A" R.TInt [R.Ri 1] (R.empty :: R.Reticolo String)
                # unsafeFromRight
      isNothing (R.getColumn "B" ret) `shouldEqual` true
    
    it "distinguishes columns with different headers" do
      let ret = R.addColumn "First" R.TInt [R.Ri 1] (R.empty :: R.Reticolo String)
                >>= R.addColumn "Second" R.TInt [R.Ri 2]
                # unsafeFromRight
      case R.getColumn "First" ret of
        Nothing -> false `shouldEqual` true
        Just col -> col.values `shouldEqual` [R.Ri 1]

  describe "Reticolo - Row Operations" do
    
    it "gets row by index" do
      let ret = R.addColumn "A" R.TInt [R.Ri 1, R.Ri 2] (R.empty :: R.Reticolo String)
                >>= R.addColumn "B" R.TString [R.Rs "x", R.Rs "y"]
                # unsafeFromRight
      case R.getRow 0 ret of
        Nothing -> false `shouldEqual` true
        Just row -> row `shouldEqual` [R.Ri 1, R.Rs "x"]
    
    it "gets different rows by index" do
      let ret = R.addColumn "A" R.TInt [R.Ri 1, R.Ri 2, R.Ri 3] (R.empty :: R.Reticolo String)
                # unsafeFromRight
      case R.getRow 1 ret of
        Nothing -> false `shouldEqual` true
        Just row -> row `shouldEqual` [R.Ri 2]
    
    it "returns Nothing for out-of-bounds row" do
      let ret = R.addColumn "A" R.TInt [R.Ri 1] (R.empty :: R.Reticolo String)
                # unsafeFromRight
      isNothing (R.getRow 5 ret) `shouldEqual` true
    
    it "returns Nothing for negative row index" do
      let ret = R.addColumn "A" R.TInt [R.Ri 1] (R.empty :: R.Reticolo String)
                # unsafeFromRight
      isNothing (R.getRow (-1) ret) `shouldEqual` true

  describe "Reticolo - Row as Map Operations" do
    
    it "gets row as Map by index" do
      let ret = R.addColumn "A" R.TInt [R.Ri 1, R.Ri 2] (R.empty :: R.Reticolo String)
                >>= R.addColumn "B" R.TString [R.Rs "x", R.Rs "y"]
                # unsafeFromRight
      case R.getMap 0 ret of
        Nothing -> false `shouldEqual` true
        Just m -> do
          Map.lookup "A" m `shouldEqual` Just (R.Ri 1)
          Map.lookup "B" m `shouldEqual` Just (R.Rs "x")
    
    it "gets different rows as Map by index" do
      let ret = R.addColumn "X" R.TInt [R.Ri 10, R.Ri 20, R.Ri 30] (R.empty :: R.Reticolo String)
                >>= R.addColumn "Y" R.TFloat [R.Rf 1.5, R.Rf 2.5, R.Rf 3.5]
                # unsafeFromRight
      case R.getMap 1 ret of
        Nothing -> false `shouldEqual` true
        Just m -> do
          Map.lookup "X" m `shouldEqual` Just (R.Ri 20)
          Map.lookup "Y" m `shouldEqual` Just (R.Rf 2.5)
    
    it "returns Nothing for out-of-bounds row in getMap" do
      let ret = R.addColumn "A" R.TInt [R.Ri 1] (R.empty :: R.Reticolo String)
                # unsafeFromRight
      isNothing (R.getMap 5 ret) `shouldEqual` true
    
    it "returns Nothing for negative row index in getMap" do
      let ret = R.addColumn "A" R.TInt [R.Ri 1] (R.empty :: R.Reticolo String)
                # unsafeFromRight
      isNothing (R.getMap (-1) ret) `shouldEqual` true
    
    it "getMap works with multiple columns" do
      let ret = R.addColumn "Name" R.TString [R.Rs "Alice", R.Rs "Bob"] (R.empty :: R.Reticolo String)
                >>= R.addColumn "Age" R.TInt [R.Ri 30, R.Ri 25]
                >>= R.addColumn "Score" R.TFloat [R.Rf 95.5, R.Rf 87.3]
                # unsafeFromRight
      case R.getMap 0 ret of
        Nothing -> false `shouldEqual` true
        Just m -> do
          Map.size m `shouldEqual` 3
          Map.lookup "Name" m `shouldEqual` Just (R.Rs "Alice")
          Map.lookup "Age" m `shouldEqual` Just (R.Ri 30)
          Map.lookup "Score" m `shouldEqual` Just (R.Rf 95.5)
    
    it "getMap works with integer headers" do
      let ret = R.addColumn 1 R.TInt [R.Ri 100, R.Ri 200] (R.empty :: R.Reticolo Int)
                >>= R.addColumn 2 R.TString [R.Rs "a", R.Rs "b"]
                # unsafeFromRight
      case R.getMap 1 ret of
        Nothing -> false `shouldEqual` true
        Just m -> do
          Map.lookup 1 m `shouldEqual` Just (R.Ri 200)
          Map.lookup 2 m `shouldEqual` Just (R.Rs "b")

  describe "Reticolo - Derived Columns" do
    
    it "creates derived column from two int columns (sum)" do
      let add (R.Ri a) (R.Ri b) = R.Ri (a + b)
          add _ _ = R.Ri 0
      let ret = R.addColumn "A" R.TInt [R.Ri 1, R.Ri 2] (R.empty :: R.Reticolo String)
                >>= R.addColumn "B" R.TInt [R.Ri 3, R.Ri 4]
                >>= R.derivedColumn "A" "B" "Sum" add
                # unsafeFromRight
      case R.getColumn "Sum" ret of
        Nothing -> false `shouldEqual` true
        Just col -> col.values `shouldEqual` [R.Ri 4, R.Ri 6]
    
    it "creates derived column from float columns (multiply)" do
      let multiply (R.Rf a) (R.Rf b) = R.Rf (a * b)
          multiply _ _ = R.Rf 0.0
      let ret = R.addColumn "Price" R.TFloat [R.Rf 2.0, R.Rf 3.0] (R.empty :: R.Reticolo String)
                >>= R.addColumn "Quantity" R.TFloat [R.Rf 5.0, R.Rf 10.0]
                >>= R.derivedColumn "Price" "Quantity" "Total" multiply
                # unsafeFromRight
      case R.getColumn "Total" ret of
        Nothing -> false `shouldEqual` true
        Just col -> col.values `shouldEqual` [R.Rf 10.0, R.Rf 30.0]
    
    it "creates derived column from mixed types" do
      let mixOp (R.Rf f) (R.Ri i) = R.Rf (f * (toNumber i))
          mixOp _ _ = R.Rf 0.0
          toNumber n = 0.0 + (if n >= 0 then 1.0 else -1.0) * go (if n >= 0 then n else -n) 0.0
            where go 0 acc = acc
                  go m acc = go (m - 1) (acc + 1.0)
      let ret = R.addColumn "F" R.TFloat [R.Rf 2.5] (R.empty :: R.Reticolo String)
                >>= R.addColumn "I" R.TInt [R.Ri 4]
                >>= R.derivedColumn "F" "I" "Result" mixOp
                # unsafeFromRight
      case R.getColumn "Result" ret of
        Nothing -> false `shouldEqual` true
        Just col -> Array.length col.values `shouldEqual` 1
    
    it "creates derived column with string concatenation" do
      let concat (R.Rs a) (R.Rs b) = R.Rs (a <> b)
          concat _ _ = R.Rs ""
      let ret = R.addColumn "First" R.TString [R.Rs "Hello", R.Rs "Good"] (R.empty :: R.Reticolo String)
                >>= R.addColumn "Second" R.TString [R.Rs " World", R.Rs "bye"]
                >>= R.derivedColumn "First" "Second" "Combined" concat
                # unsafeFromRight
      case R.getColumn "Combined" ret of
        Nothing -> false `shouldEqual` true
        Just col -> col.values `shouldEqual` [R.Rs "Hello World", R.Rs "Goodbye"]

  describe "Reticolo - Aggregations: Sum" do
    
    it "computes sum of integer column" do
      let ret = R.addColumn "Values" R.TInt [R.Ri 1, R.Ri 2, R.Ri 3] (R.empty :: R.Reticolo String)
                >>= R.aggregate "Values" "Sum" R.Sum
                # unsafeFromRight
      case R.getColumn "Sum" ret of
        Nothing -> false `shouldEqual` true
        Just col -> col.values `shouldEqual` [R.Rf 6.0, R.Rf 6.0, R.Rf 6.0]
    
    it "computes sum of float column" do
      let ret = R.addColumn "Values" R.TFloat [R.Rf 1.5, R.Rf 2.5, R.Rf 3.0] (R.empty :: R.Reticolo String)
                >>= R.aggregate "Values" "Sum" R.Sum
                # unsafeFromRight
      case R.getColumn "Sum" ret of
        Nothing -> false `shouldEqual` true
        Just col -> col.values `shouldEqual` [R.Rf 7.0, R.Rf 7.0, R.Rf 7.0]
    
    it "computes sum of empty column" do
      let ret = R.addColumn "Values" R.TInt [] (R.empty :: R.Reticolo String)
                >>= R.aggregate "Values" "Sum" R.Sum
                # unsafeFromRight
      R.rowCount ret `shouldEqual` 0

  describe "Reticolo - Aggregations: Count" do
    
    it "counts rows in column" do
      let ret = R.addColumn "Values" R.TInt [R.Ri 10, R.Ri 20, R.Ri 30] (R.empty :: R.Reticolo String)
                >>= R.aggregate "Values" "Count" R.Count
                # unsafeFromRight
      case R.getColumn "Count" ret of
        Nothing -> false `shouldEqual` true
        Just col -> col.values `shouldEqual` [R.Ri 3, R.Ri 3, R.Ri 3]
    
    it "counts single row" do
      let ret = R.addColumn "Values" R.TString [R.Rs "only"] (R.empty :: R.Reticolo String)
                >>= R.aggregate "Values" "Count" R.Count
                # unsafeFromRight
      case R.getColumn "Count" ret of
        Nothing -> false `shouldEqual` true
        Just col -> col.values `shouldEqual` [R.Ri 1]

  describe "Reticolo - Aggregations: Average" do
    
    it "computes average of integer column" do
      let ret = R.addColumn "Values" R.TInt [R.Ri 10, R.Ri 20, R.Ri 30] (R.empty :: R.Reticolo String)
                >>= R.aggregate "Values" "Avg" R.Avg
                # unsafeFromRight
      case R.getColumn "Avg" ret of
        Nothing -> false `shouldEqual` true
        Just col -> col.values `shouldEqual` [R.Rf 20.0, R.Rf 20.0, R.Rf 20.0]
    
    it "computes average of float column" do
      let ret = R.addColumn "Values" R.TFloat [R.Rf 1.0, R.Rf 2.0, R.Rf 3.0, R.Rf 4.0] (R.empty :: R.Reticolo String)
                >>= R.aggregate "Values" "Avg" R.Avg
                # unsafeFromRight
      case R.getColumn "Avg" ret of
        Nothing -> false `shouldEqual` true
        Just col -> col.values `shouldEqual` [R.Rf 2.5, R.Rf 2.5, R.Rf 2.5, R.Rf 2.5]

  describe "Reticolo - Aggregations: Min" do
    
    it "finds minimum of integer column" do
      let ret = R.addColumn "Values" R.TInt [R.Ri 30, R.Ri 10, R.Ri 20] (R.empty :: R.Reticolo String)
                >>= R.aggregate "Values" "Min" R.Min
                # unsafeFromRight
      case R.getColumn "Min" ret of
        Nothing -> false `shouldEqual` true
        Just col -> col.values `shouldEqual` [R.Ri 10, R.Ri 10, R.Ri 10]
    
    it "finds minimum of float column" do
      let ret = R.addColumn "Values" R.TFloat [R.Rf 5.5, R.Rf 1.2, R.Rf 3.3] (R.empty :: R.Reticolo String)
                >>= R.aggregate "Values" "Min" R.Min
                # unsafeFromRight
      case R.getColumn "Min" ret of
        Nothing -> false `shouldEqual` true
        Just col -> col.values `shouldEqual` [R.Rf 1.2, R.Rf 1.2, R.Rf 1.2]

  describe "Reticolo - Aggregations: Max" do
    
    it "finds maximum of integer column" do
      let ret = R.addColumn "Values" R.TInt [R.Ri 30, R.Ri 10, R.Ri 20] (R.empty :: R.Reticolo String)
                >>= R.aggregate "Values" "Max" R.Max
                # unsafeFromRight
      case R.getColumn "Max" ret of
        Nothing -> false `shouldEqual` true
        Just col -> col.values `shouldEqual` [R.Ri 30, R.Ri 30, R.Ri 30]
    
    it "finds maximum of float column" do
      let ret = R.addColumn "Values" R.TFloat [R.Rf 5.5, R.Rf 1.2, R.Rf 9.9] (R.empty :: R.Reticolo String)
                >>= R.aggregate "Values" "Max" R.Max
                # unsafeFromRight
      case R.getColumn "Max" ret of
        Nothing -> false `shouldEqual` true
        Just col -> col.values `shouldEqual` [R.Rf 9.9, R.Rf 9.9, R.Rf 9.9]

  describe "Reticolo - Multiple Aggregations" do
    
    it "applies multiple aggregations to same column" do
      let ret = R.addColumn "Data" R.TInt [R.Ri 10, R.Ri 20, R.Ri 30, R.Ri 40] (R.empty :: R.Reticolo String)
                >>= R.aggregate "Data" "Sum" R.Sum
                >>= R.aggregate "Data" "Count" R.Count
                >>= R.aggregate "Data" "Avg" R.Avg
                >>= R.aggregate "Data" "Min" R.Min
                >>= R.aggregate "Data" "Max" R.Max
                # unsafeFromRight
      R.columnCount ret `shouldEqual` 6

  describe "Reticolo - Inner Join" do
    
    it "joins tables with matching values on common column" do
      let retA = R.addColumn "ID" R.TInt [R.Ri 1, R.Ri 2, R.Ri 3] (R.empty :: R.Reticolo String)
                 >>= R.addColumn "Name" R.TString [R.Rs "Alice", R.Rs "Bob", R.Rs "Carol"]
                 # unsafeFromRight
      let retB = R.addColumn "ID" R.TInt [R.Ri 2, R.Ri 3, R.Ri 4] (R.empty :: R.Reticolo String)
                 >>= R.addColumn "Age" R.TInt [R.Ri 25, R.Ri 30, R.Ri 35]
                 # unsafeFromRight
      let result = R.innerJoin retA retB
      R.rowCount result `shouldEqual` 2
      R.columnCount result `shouldEqual` 3
      case R.getRow 0 result of
        Nothing -> false `shouldEqual` true
        Just row -> row `shouldEqual` [R.Ri 2, R.Rs "Bob", R.Ri 25]
      case R.getRow 1 result of
        Nothing -> false `shouldEqual` true
        Just row -> row `shouldEqual` [R.Ri 3, R.Rs "Carol", R.Ri 30]
    
    it "returns empty when no matching values" do
      let retA = R.addColumn "ID" R.TInt [R.Ri 1, R.Ri 2] (R.empty :: R.Reticolo String)
                 # unsafeFromRight
      let retB = R.addColumn "ID" R.TInt [R.Ri 3, R.Ri 4] (R.empty :: R.Reticolo String)
                 # unsafeFromRight
      let result = R.innerJoin retA retB
      R.rowCount result `shouldEqual` 0
    
    it "joins on multiple common columns" do
      let retA = R.addColumn "A" R.TInt [R.Ri 1, R.Ri 1, R.Ri 2] (R.empty :: R.Reticolo String)
                 >>= R.addColumn "B" R.TString [R.Rs "x", R.Rs "y", R.Rs "x"]
                 >>= R.addColumn "DataA" R.TFloat [R.Rf 1.0, R.Rf 2.0, R.Rf 3.0]
                 # unsafeFromRight
      let retB = R.addColumn "A" R.TInt [R.Ri 1, R.Ri 2] (R.empty :: R.Reticolo String)
                 >>= R.addColumn "B" R.TString [R.Rs "x", R.Rs "x"]
                 >>= R.addColumn "DataB" R.TInt [R.Ri 100, R.Ri 200]
                 # unsafeFromRight
      let result = R.innerJoin retA retB
      R.rowCount result `shouldEqual` 2
      R.columnCount result `shouldEqual` 4
      case R.getRow 0 result of
        Nothing -> false `shouldEqual` true
        Just row -> row `shouldEqual` [R.Ri 1, R.Rs "x", R.Rf 1.0, R.Ri 100]
      case R.getRow 1 result of
        Nothing -> false `shouldEqual` true
        Just row -> row `shouldEqual` [R.Ri 2, R.Rs "x", R.Rf 3.0, R.Ri 200]
    
    it "handles one-to-many relationships" do
      let retA = R.addColumn "ID" R.TInt [R.Ri 1] (R.empty :: R.Reticolo String)
                 >>= R.addColumn "Name" R.TString [R.Rs "Alice"]
                 # unsafeFromRight
      let retB = R.addColumn "ID" R.TInt [R.Ri 1, R.Ri 1, R.Ri 1] (R.empty :: R.Reticolo String)
                 >>= R.addColumn "Item" R.TString [R.Rs "Book", R.Rs "Pen", R.Rs "Paper"]
                 # unsafeFromRight
      let result = R.innerJoin retA retB
      R.rowCount result `shouldEqual` 3
      case R.getColumn "Name" result of
        Nothing -> false `shouldEqual` true
        Just col -> col.values `shouldEqual` [R.Rs "Alice", R.Rs "Alice", R.Rs "Alice"]
    
    it "handles many-to-many relationships" do
      let retA = R.addColumn "ID" R.TInt [R.Ri 1, R.Ri 1, R.Ri 2] (R.empty :: R.Reticolo String)
                 # unsafeFromRight
      let retB = R.addColumn "ID" R.TInt [R.Ri 1, R.Ri 1] (R.empty :: R.Reticolo String)
                 # unsafeFromRight
      let result = R.innerJoin retA retB
      R.rowCount result `shouldEqual` 4
    
    it "preserves non-common columns from both tables" do
      let retA = R.addColumn "ID" R.TInt [R.Ri 1] (R.empty :: R.Reticolo String)
                 >>= R.addColumn "Left1" R.TString [R.Rs "a"]
                 >>= R.addColumn "Left2" R.TInt [R.Ri 10]
                 # unsafeFromRight
      let retB = R.addColumn "ID" R.TInt [R.Ri 1] (R.empty :: R.Reticolo String)
                 >>= R.addColumn "Right1" R.TString [R.Rs "b"]
                 >>= R.addColumn "Right2" R.TInt [R.Ri 20]
                 # unsafeFromRight
      let result = R.innerJoin retA retB
      R.columnCount result `shouldEqual` 5
      case R.getRow 0 result of
        Nothing -> false `shouldEqual` true
        Just row -> row `shouldEqual` [R.Ri 1, R.Rs "a", R.Ri 10, R.Rs "b", R.Ri 20]

  describe "Reticolo - Cartesian Product" do
    
    it "performs cartesian product with no common headers" do
      let retA = R.addColumn "Name" R.TString [R.Rs "Alice", R.Rs "Bob"] (R.empty :: R.Reticolo String)
                 # unsafeFromRight
      let retB = R.addColumn "Price" R.TInt [R.Ri 10, R.Ri 20] (R.empty :: R.Reticolo String)
                 # unsafeFromRight
      let result = R.cartesian retA retB
      R.rowCount result `shouldEqual` 4
      R.columnCount result `shouldEqual` 2
      case R.getRow 0 result of
        Nothing -> false `shouldEqual` true
        Just row -> row `shouldEqual` [R.Rs "Alice", R.Ri 10]
      case R.getRow 1 result of
        Nothing -> false `shouldEqual` true
        Just row -> row `shouldEqual` [R.Rs "Alice", R.Ri 20]
      case R.getRow 2 result of
        Nothing -> false `shouldEqual` true
        Just row -> row `shouldEqual` [R.Rs "Bob", R.Ri 10]
      case R.getRow 3 result of
        Nothing -> false `shouldEqual` true
        Just row -> row `shouldEqual` [R.Rs "Bob", R.Ri 20]
    
    it "cartesian product with different sized tables" do
      let retA = R.addColumn "A" R.TInt [R.Ri 1, R.Ri 2, R.Ri 3] (R.empty :: R.Reticolo String)
                 # unsafeFromRight
      let retB = R.addColumn "B" R.TString [R.Rs "x", R.Rs "y"] (R.empty :: R.Reticolo String)
                 # unsafeFromRight
      let result = R.cartesian retA retB
      R.rowCount result `shouldEqual` 6
      R.columnCount result `shouldEqual` 2
    
    it "cartesian product with multiple columns on each side" do
      let retA = R.addColumn "ID" R.TInt [R.Ri 1, R.Ri 2] (R.empty :: R.Reticolo String)
                 >>= R.addColumn "Name" R.TString [R.Rs "Alice", R.Rs "Bob"]
                 # unsafeFromRight
      let retB = R.addColumn "Product" R.TString [R.Rs "Book", R.Rs "Pen"] (R.empty :: R.Reticolo String)
                 >>= R.addColumn "Price" R.TFloat [R.Rf 10.5, R.Rf 2.0]
                 # unsafeFromRight
      let result = R.cartesian retA retB
      R.rowCount result `shouldEqual` 4
      R.columnCount result `shouldEqual` 4
      case R.getRow 0 result of
        Nothing -> false `shouldEqual` true
        Just row -> row `shouldEqual` [R.Ri 1, R.Rs "Alice", R.Rs "Book", R.Rf 10.5]
    
    it "cartesian product with single row tables" do
      let retA = R.addColumn "X" R.TInt [R.Ri 5] (R.empty :: R.Reticolo String)
                 # unsafeFromRight
      let retB = R.addColumn "Y" R.TInt [R.Ri 10] (R.empty :: R.Reticolo String)
                 # unsafeFromRight
      let result = R.cartesian retA retB
      R.rowCount result `shouldEqual` 1
      case R.getRow 0 result of
        Nothing -> false `shouldEqual` true
        Just row -> row `shouldEqual` [R.Ri 5, R.Ri 10]

  describe "Reticolo - Union" do
    
    it "unions two tables with identical headers and no duplicates" do
      let retA = R.addColumn "ID" R.TInt [R.Ri 1, R.Ri 2] (R.empty :: R.Reticolo String)
                 >>= R.addColumn "Name" R.TString [R.Rs "Alice", R.Rs "Bob"]
                 # unsafeFromRight
      let retB = R.addColumn "ID" R.TInt [R.Ri 3, R.Ri 4] (R.empty :: R.Reticolo String)
                 >>= R.addColumn "Name" R.TString [R.Rs "Carol", R.Rs "Dave"]
                 # unsafeFromRight
      let result = R.union retA retB
      R.rowCount result `shouldEqual` 4
      R.columnCount result `shouldEqual` 2
    
    it "unions and deduplicates identical rows" do
      let retA = R.addColumn "A" R.TInt [R.Ri 1, R.Ri 2, R.Ri 3] (R.empty :: R.Reticolo String)
                 >>= R.addColumn "B" R.TString [R.Rs "x", R.Rs "y", R.Rs "z"]
                 # unsafeFromRight
      let retB = R.addColumn "A" R.TInt [R.Ri 2, R.Ri 4] (R.empty :: R.Reticolo String)
                 >>= R.addColumn "B" R.TString [R.Rs "y", R.Rs "w"]
                 # unsafeFromRight
      let result = R.union retA retB
      R.rowCount result `shouldEqual` 4  -- 1,x + 2,y + 3,z + 4,w (2,y deduplicated)
      case R.getColumn "A" result of
        Nothing -> false `shouldEqual` true
        Just col -> Array.sort col.values `shouldEqual` [R.Ri 1, R.Ri 2, R.Ri 3, R.Ri 4]
    
    it "unions with all rows identical" do
      let retA = R.addColumn "X" R.TInt [R.Ri 1, R.Ri 2] (R.empty :: R.Reticolo String)
                 # unsafeFromRight
      let retB = R.addColumn "X" R.TInt [R.Ri 1, R.Ri 2] (R.empty :: R.Reticolo String)
                 # unsafeFromRight
      let result = R.union retA retB
      R.rowCount result `shouldEqual` 2
    
    it "returns empty when headers don't match" do
      let retA = R.addColumn "A" R.TInt [R.Ri 1] (R.empty :: R.Reticolo String)
                 # unsafeFromRight
      let retB = R.addColumn "B" R.TInt [R.Ri 2] (R.empty :: R.Reticolo String)
                 # unsafeFromRight
      let result = R.union retA retB
      R.rowCount result `shouldEqual` 0
      R.columnCount result `shouldEqual` 0
    
    it "unions with multiple columns and partial overlap" do
      let retA = R.addColumn "A" R.TInt [R.Ri 1, R.Ri 2] (R.empty :: R.Reticolo String)
                 >>= R.addColumn "B" R.TString [R.Rs "a", R.Rs "b"]
                 >>= R.addColumn "C" R.TFloat [R.Rf 1.0, R.Rf 2.0]
                 # unsafeFromRight
      let retB = R.addColumn "A" R.TInt [R.Ri 2, R.Ri 3] (R.empty :: R.Reticolo String)
                 >>= R.addColumn "B" R.TString [R.Rs "b", R.Rs "c"]
                 >>= R.addColumn "C" R.TFloat [R.Rf 2.0, R.Rf 3.0]
                 # unsafeFromRight
      let result = R.union retA retB
      R.rowCount result `shouldEqual` 3  -- (1,a,1.0), (2,b,2.0), (3,c,3.0)
    
    it "unions empty tables" do
      let retA = R.addColumn "X" R.TInt [] (R.empty :: R.Reticolo String)
                 # unsafeFromRight
      let retB = R.addColumn "X" R.TInt [] (R.empty :: R.Reticolo String)
                 # unsafeFromRight
      let result = R.union retA retB
      R.rowCount result `shouldEqual` 0

  describe "Reticolo - Edge Cases" do
    
    it "handles single row table" do
      let ret = R.addColumn "A" R.TInt [R.Ri 42] (R.empty :: R.Reticolo String)
                # unsafeFromRight
      R.rowCount ret `shouldEqual` 1
      case R.getRow 0 ret of
        Nothing -> false `shouldEqual` true
        Just row -> row `shouldEqual` [R.Ri 42]
    
    it "handles single column table" do
      let ret = R.addColumn "Only" R.TString [R.Rs "a", R.Rs "b"] (R.empty :: R.Reticolo String)
                # unsafeFromRight
      R.columnCount ret `shouldEqual` 1
    
    it "handles table with only empty columns" do
      let ret = R.addColumn "A" R.TInt [] (R.empty :: R.Reticolo String)
                >>= R.addColumn "B" R.TInt []
                # unsafeFromRight
      R.rowCount ret `shouldEqual` 0
      R.columnCount ret `shouldEqual` 2
    
    it "allows removing all columns" do
      let ret = R.addColumn "A" R.TInt [R.Ri 1] (R.empty :: R.Reticolo String)
                # unsafeFromRight
                # R.removeColumn "A"
      R.columnCount ret `shouldEqual` 0
    
    it "handles boolean column operations" do
      let ret = R.addColumn "Flags" R.TBool [R.Rb true, R.Rb false, R.Rb true] (R.empty :: R.Reticolo String)
                # unsafeFromRight
      case R.getColumn "Flags" ret of
        Nothing -> false `shouldEqual` true
        Just col -> Array.length col.values `shouldEqual` 3

  describe "Reticolo - Integer Header Types" do
    
    it "uses integer headers" do
      let ret = R.addColumn 1 R.TInt [R.Ri 10] (R.empty :: R.Reticolo Int)
                >>= R.addColumn 2 R.TString [R.Rs "test"]
                # unsafeFromRight
      case R.getColumn 1 ret of
        Nothing -> false `shouldEqual` true
        Just col -> col.header `shouldEqual` 1
    
    it "joins tables with integer headers" do
      let ret1 = R.addColumn 1 R.TInt [R.Ri 10, R.Ri 20] (R.empty :: R.Reticolo Int)
                 >>= R.addColumn 2 R.TString [R.Rs "a", R.Rs "b"]
                 # unsafeFromRight
      let ret2 = R.addColumn 1 R.TInt [R.Ri 20, R.Ri 30] (R.empty :: R.Reticolo Int)
                 >>= R.addColumn 3 R.TFloat [R.Rf 1.5, R.Rf 2.5]
                 # unsafeFromRight
      let joined = R.innerJoin ret1 ret2
      R.rowCount joined `shouldEqual` 1
      case R.getRow 0 joined of
        Nothing -> false `shouldEqual` true
        Just row -> row `shouldEqual` [R.Ri 20, R.Rs "b", R.Rf 1.5]

  describe "Reticolo - Complex Workflows" do
    
    it "complete workflow: build, derive, aggregate, merge" do
      -- Build initial table
      let sales = R.addColumn "Product" R.TString [R.Rs "A", R.Rs "B"] (R.empty :: R.Reticolo String)
                  >>= R.addColumn "Price" R.TFloat [R.Rf 10.0, R.Rf 20.0]
                  >>= R.addColumn "Qty" R.TInt [R.Ri 5, R.Ri 3]
                  # unsafeFromRight
      
      -- Add derived column
      let multiply (R.Rf p) (R.Ri q) = R.Rf (p * toNum q)
          multiply _ _ = R.Rf 0.0
          toNum n = 0.0 + (if n >= 0 then 1.0 else -1.0) * go (if n >= 0 then n else -n) 0.0
            where go 0 acc = acc
                  go m acc = go (m - 1) (acc + 1.0)
      let withTotal = sales # Right >>= R.derivedColumn "Price" "Qty" "Total" multiply # unsafeFromRight
      
      -- Aggregate
      let withSum = withTotal # Right >>= R.aggregate "Total" "Revenue" R.Sum # unsafeFromRight
      
      -- Build second table
      let costs = R.addColumn "Category" R.TString [R.Rs "Electronics"] (R.empty :: R.Reticolo String)
                  >>= R.addColumn "Cost" R.TFloat [R.Rf 5.0]
                  # unsafeFromRight
      
      -- Cartesian product since no common columns
      let merged = R.cartesian withSum costs
      
      -- Verify
      (R.getColumn "Product" merged) `shouldSatisfy` (\x -> x /= Nothing)
      R.columnCount merged `shouldEqual` 7  -- Product, Price, Qty, Total, Revenue + Category, Cost
    
    it "chains multiple derived columns" do
      let ret = R.addColumn "A" R.TInt [R.Ri 2, R.Ri 4] (R.empty :: R.Reticolo String)
                >>= R.addColumn "B" R.TInt [R.Ri 3, R.Ri 5]
                # unsafeFromRight
      
      let add (R.Ri a) (R.Ri b) = R.Ri (a + b)
          add _ _ = R.Ri 0
      let mult (R.Ri a) (R.Ri b) = R.Ri (a * b)
          mult _ _ = R.Ri 0
      
      let withSum = ret # Right >>= R.derivedColumn "A" "B" "Sum" add # unsafeFromRight
      let withProd = withSum # Right >>= R.derivedColumn "A" "B" "Product" mult # unsafeFromRight
      
      R.columnCount withProd `shouldEqual` 4
      case R.getColumn "Sum" withProd of
        Nothing -> false `shouldEqual` true
        Just col -> col.values `shouldEqual` [R.Ri 5, R.Ri 9]
      case R.getColumn "Product" withProd of
        Nothing -> false `shouldEqual` true
        Just col -> col.values `shouldEqual` [R.Ri 6, R.Ri 20]
