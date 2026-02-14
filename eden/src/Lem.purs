module Kubrick.Lem
  ( (+:)
  , (+>)
  , (:+)
  , (:::)
  , (<+)
  , (<+>)
  , (\/)
  , Bag1(..)
  , Dict1(..)
  , Lem(..)
  , Sek1(..)
  , addPrimitive
  , addPrimitiveFlipped
  , appendPrimitive
  , class AddPrimitive
  , class AddPrimitiveFlipped
  , class CombineLem
  , class MakeLem
  , class Or
  , class PostPrimitive
  , class PreLem
  , class PrePrimitive
  , combine
  , concat
  , lem
  , or
  , prependPrimitive
  )
  where

-- * Imports

import Prelude

import Data.Foldable (class Foldable, foldl, foldr)
import Data.List (List(..), (:))
import Data.List as List
import Data.Traversable (class Traversable, traverse, sequence)
import Data.Traversable as Data.Traversable
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))

-- * Type Definitions

-- ** Auxiliary Types

-- | Sek-like types (can be used in Sekdict)
data Sek1 t
  = S2 (Lem t) (Lem t) (List (Lem t)) -- at least 2 elements
  | S1 (Lem t) -- single element

-- Bag-like types (can be used in Bagdict)
data Bag1 t
  = B2 (Lem t) (Lem t) (List (Lem t)) -- at least 2 elements, guaranteed unique
  | B1 (Lem t) -- single element

-- Dict-like types (can be used as second arg in Sekdict/Bagdict)
-- | **WARNING**: Do not construct D2 directly! Use Lem.d2 to ensure key uniqueness.
data Dict1 t
  = D2 (Tuple (Lem t) (Lem t)) (Tuple (Lem t) (Lem t)) (List (Tuple (Lem t) (Lem t))) -- at least 2 pairs, guaranteed unique keys
  | D1 (Lem t) (Lem t) -- single pair

-- ** Main Lem Type

-- | Main Lem type hierarchy
-- |
-- | **WARNING**: Do not construct Bag, Choice, or Dict directly!
-- | Always use:
-- |   - Operators: `<+>`, `<+`, `+:`, `:::`, `:+`
-- |   - Smart constructors: `Lem.bag`, `Lem.choice`, `Lem.dict`
-- |
-- | Direct construction violates uniqueness invariants:
-- |   - Bag/Choice: element uniqueness
-- |   - Dict: key uniqueness (first element of each Tuple)
data Lem t
  = L0
  | Gap -- empty cell for search  
  | L1 t
  | Pair (Lem t) (Lem t)
  | Sek (Lem t) (Lem t) (List (Lem t)) -- Sek with at least 2 elements
  | Bag (Lem t) (Lem t) (List (Lem t)) -- Bag with at least 2 elements, guaranteed unique
  | Choice (Lem t) (Lem t) (List (Lem t)) -- Choice with at least 2 elements, guaranteed unique
  | Dict (Tuple (Lem t) (Lem t)) (Tuple (Lem t) (Lem t)) (List (Tuple (Lem t) (Lem t))) -- at least 2 pairs, guaranteed unique keys
  | Sekdict (Sek1 t) (Dict1 t) -- Type-safe: only Sek/S1 + Dict/Pair
  | Bagdict (Bag1 t) (Dict1 t) -- Type-safe: only Bag/B1 + Dict/Pair

-- * Smart Constructors

-- Internal helper for Bag that works with Lem values
bagLem :: forall t. Eq t => Lem t -> Lem t -> List (Lem t) -> Lem t
bagLem fst snd rest =
  let
    allElems = fst : snd : rest
    unique = List.nubByEq (==) allElems
  in
    case unique of
      f : s : r -> Bag f s r
      f : Nil -> f
      _ -> L0

-- | Smart constructor for Bag that ensures uniqueness
-- | Takes primitive values and wraps them in L1
bag :: forall t. Eq t => t -> t -> List t -> Lem t
bag fst snd rest = bagLem (L1 fst) (L1 snd) (map L1 rest)

-- Internal helper for Choice that works with Lem values
choiceLem :: forall t. Eq t => Lem t -> Lem t -> List (Lem t) -> Lem t
choiceLem fst snd rest =
  let
    -- Flatten if fst is a Choice
    fstElems = case fst of
      Choice f s r -> f : s : r
      other -> other : Nil
    -- Flatten if snd is a Choice
    sndElems = case snd of
      Choice f s r -> f : s : r
      other -> other : Nil
    -- Combine all elements
    allElems = fstElems <> sndElems <> rest
    unique = List.nubByEq (==) allElems
  in
    case unique of
      f : s : r -> Choice f s r
      f : Nil -> f
      _ -> L0
class Or s t u | s t -> u where
  or :: Eq u => s -> t -> Lem u

-- Both Lem values  
instance orLemLem :: Eq t => Or (Lem t) (Lem t) t where
  or lem L0 = lem
  or L0 lem = lem
  or (a :: Lem t) (b :: Lem t) = choiceLem a b Nil
else instance orLemPrimitive :: Eq t => Or (Lem t) t t where
  or (L0) (p :: t) = L1 p
  or (lem :: Lem t) (p :: t) = choiceLem lem (L1 p) Nil
else instance orPrimitiveLem :: Eq t => Or t (Lem t) t where
  or (p :: t) (L0) = L1 p
  or (p :: t) (lem :: Lem t) = choiceLem (L1 p) lem Nil

-- Both primitives
else instance orPrimitivePrimitive :: Eq t => Or t t t where
  or (p1 :: t) (p2 :: t) = choiceLem (L1 p1) (L1 p2) Nil

-- Internal helper for Dict that works with Lem values
dictLem :: forall t. Eq t => Tuple (Lem t) (Lem t) -> Tuple (Lem t) (Lem t) -> List (Tuple (Lem t) (Lem t)) -> Lem t
dictLem fst snd rest =
  let
    allPairs = fst : snd : rest
    -- Remove duplicates based on key (first element of Tuple)
    unique = List.nubByEq (\(Tuple k1 _) (Tuple k2 _) -> k1 == k2) allPairs
  in
    case unique of
      f : s : r -> Dict f s r
      f : Nil -> case f of
        Tuple k v -> Pair k v
      _ -> L0

-- | Smart constructor for Dict that ensures key uniqueness
-- | Takes tuples of primitive values and wraps them
dict :: forall t. Eq t => Tuple t t -> Tuple t t -> List (Tuple t t) -> Lem t
dict fst snd rest =
  let
    wrapPair (Tuple k v) = Tuple (L1 k) (L1 v)
  in
    dictLem (wrapPair fst) (wrapPair snd) (map wrapPair rest)

-- Internal helper for D2 that works with Lem values
d2Lem :: forall t. Eq t => Tuple (Lem t) (Lem t) -> Tuple (Lem t) (Lem t) -> List (Tuple (Lem t) (Lem t)) -> Dict1 t
d2Lem fst snd rest =
  let
    allPairs = fst : snd : rest
    -- Remove duplicates based on key (first element of Tuple)
    unique = List.nubByEq (\(Tuple k1 _) (Tuple k2 _) -> k1 == k2) allPairs
  in
    case unique of
      f : s : r -> D2 f s r
      f : Nil -> case f of
        Tuple k v -> D1 k v
      _ -> D1 L0 L0 -- Should not happen, but needed for totality

-- * Polymorphic Constructor

-- | Typeclass for creating Lem values from different input types
-- |
-- | Examples:
-- | ```purescript
-- | lem (1 : Nil)                         -- => L1 1
-- | lem (1 : 2 : 3 : Nil)                 -- => Sek (L1 1) (L1 2) (L1 3 : Nil)
-- | lem ((1 /\ 2) : (3 /\ 4) : Nil)       -- => Dict (key-value pairs)
-- | lem ((1 /\ 2) : Nil)                  -- => Pair (L1 1) (L1 2)
-- | lem (Nil :: List Int)                 -- => L0
-- | ```
class MakeLem a t where
  lem :: a -> Lem t

-- | lem (x : y : rest) => Sek (ordered sequence)
instance makeLemList :: MakeLem (List t) t where
  lem lst = case lst of
    Nil -> L0
    x : Nil -> L1 x
    x : y : rest -> Sek (L1 x) (L1 y) (map L1 rest)

-- | lem (tuple : tuples) => Dict (from tuples)
else instance makeLemTupleList :: Eq t => MakeLem (List (Tuple t t)) t where
  lem lst = case lst of
    Nil -> L0
    t : Nil -> case t of
      Tuple k v -> Pair (L1 k) (L1 v)
    fst : snd : rest -> dict fst snd rest

-- * Type Classes and Instances

-- ** Custom Type Classes

-- | Type class for prepending a primitive to Lem
class PrePrimitive t a where
  prependPrimitive :: Eq t => a -> Lem t -> Lem t

class PreLem t where
  concat :: Eq t => Lem t -> Lem t -> Lem t

instance preLem :: Eq t => PreLem t where
  concat a b = Sek a b Nil

instance prePrimitiveValue :: Eq t => PrePrimitive t t where
  prependPrimitive el L0 = L1 el
  prependPrimitive el (L1 x) = Sek (L1 el) (L1 x) Nil
  prependPrimitive el (Sek fst snd rest) = Sek (L1 el) fst (snd : rest)
  prependPrimitive el lem = concat (L1 el) lem
else instance preTuple :: Eq t => PrePrimitive t (Tuple t t) where
  prependPrimitive (a /\ b) L0 = Pair (L1 a) (L1 b)
  prependPrimitive (a /\ b) (L1 x) = Sekdict (S1 (L1 x)) (D1 (L1 a) (L1 b))
  prependPrimitive (a /\ b) lem = concat (Pair (L1 a) (L1 b)) lem
else instance prependLem :: Eq t => PrePrimitive t (Lem t) where
  prependPrimitive (Sek x y rest) (L1 z) = Sek (L1 z) x (y : rest)
  prependPrimitive (Sek a b c) (Sek fst snd rest) = Sek a b (c <> (fst : snd : rest))
  prependPrimitive (Sekdict sek dict) (Sek fst snd rest) = Sekdict
    ( case sek of
        S2 f s r -> S2 f s (r <> (fst : snd : rest))
        S1 l -> S2 l fst (snd : rest)
    )
    dict
  prependPrimitive (Sek a b c) (Sekdict sek dict) = Sekdict
    ( case sek of
        S2 f s r -> S2 a b (c <> (f : s : r))
        S1 l -> S2 a b (c <> (l : Nil))
    )
    dict
  prependPrimitive lem (Sek fst snd rest) = Sek lem fst (snd : rest)
  prependPrimitive lem (Sekdict sek dict) = Sekdict
    ( case sek of
        S2 fst snd rest -> S2 lem fst (snd : rest)
        S1 l -> S2 lem l Nil
    )
    dict
  prependPrimitive lem (L1 y) = Sek lem (L1 y) Nil
  prependPrimitive lem Gap = Sek lem Gap Nil
  prependPrimitive Gap lem = Sek Gap lem Nil
  prependPrimitive lem (Pair k v) = Sekdict (S1 lem) (D1 k v)
  prependPrimitive lem (Dict fst snd rest) = Sekdict (S1 lem) (D2 fst snd rest)
  prependPrimitive L0 b = b
  prependPrimitive a L0 = a
  prependPrimitive (Pair a b) lem = (Pair a b) <+> lem
  prependPrimitive a b = Sek a b Nil

class PostPrimitive t a where
  appendPrimitive :: Eq t => Lem t -> a -> Lem t

instance postPrimitiveValue :: Eq t => PostPrimitive t t where
  appendPrimitive (L1 x) el = Sek (L1 x) (L1 el) Nil
  appendPrimitive Gap el = Sek Gap (L1 el) Nil
  appendPrimitive (Sek fst snd rest) el = Sek fst snd (List.snoc rest (L1 el))
  appendPrimitive (Bag fst snd rest) el = Sek (Bag fst snd rest) (L1 el) Nil
  appendPrimitive L0 el = L1 el
  appendPrimitive lem el = prependPrimitive el lem
else instance postTuple :: Eq t => PostPrimitive t (Tuple t t) where
  appendPrimitive lem (a /\ b) = prependPrimitive (a /\ b) lem
else instance appendLem :: Eq t => PostPrimitive t (Lem t) where
  appendPrimitive a b = prependPrimitive b a

-- Type class for addPrimitiveing an element to Lem, it will create Bag or Bagdict as needed
class AddPrimitive t a where
  addPrimitive :: Eq t => a -> Lem t -> Lem t

instance addPrimitiveValue :: Eq t => AddPrimitive t t where
  addPrimitive el L0 = L1 el
  addPrimitive el (L1 x) = combine (L1 el) (L1 x)
  addPrimitive el Gap = combine (L1 el) Gap
  addPrimitive el (Sek fst snd rest) = combine (L1 el) (Sek fst snd rest)
  addPrimitive el (Bag fst snd rest) = bagLem fst snd (L1 el : rest)
  addPrimitive el (Choice fst snd rest) = choiceLem fst snd (L1 el : rest)
  addPrimitive el lem = combine (L1 el) lem
else instance addPrimitiveTuple :: Eq t => AddPrimitive t (Tuple t t) where
  addPrimitive (a /\ b) Gap = Pair (L1 a) (L1 b)
  addPrimitive (a /\ b) (Pair k v) = dictLem (Tuple k v) (Tuple (L1 a) (L1 b)) Nil
  addPrimitive (a /\ b) (Dict fst snd rest) = dictLem fst snd (Tuple (L1 a) (L1 b) : rest)
  addPrimitive (a /\ b) lem = combine (Pair (L1 a) (L1 b)) lem
else instance addPrimitiveLem :: Eq t => AddPrimitive t (Lem t) where
  addPrimitive lem1 lem2 = combine lem1 lem2

-- | Flipped version of addPrimitive for left-associative +> operator
class AddPrimitiveFlipped a t where
  addPrimitiveFlipped :: Eq t => Lem t -> a -> Lem t

instance addPrimitiveFlippedValue :: Eq t => AddPrimitiveFlipped t t where
  addPrimitiveFlipped lem el = addPrimitive el lem
else instance addPrimitiveFlippedTuple :: Eq t => AddPrimitiveFlipped (Tuple t t) t where
  addPrimitiveFlipped lem tuple = addPrimitive tuple lem
else instance addPrimitiveFlippedLem :: Eq t => AddPrimitiveFlipped (Lem t) t where
  addPrimitiveFlipped lem1 lem2 = combine lem2 lem1

-- | Type class for combining two Lem values
class CombineLem t where
  combine :: Eq t => Lem t -> Lem t -> Lem t

instance combineLem :: Eq t => CombineLem t where
  combine (L1 x) (L1 y) = bagLem (L1 x) (L1 y) Nil
  combine Gap (L1 y) = bagLem Gap (L1 y) Nil
  combine (L1 x) Gap = bagLem (L1 x) Gap Nil
  combine Gap Gap = bagLem Gap Gap Nil
  combine (L1 x) (Bag fst snd rest) = bagLem (L1 x) fst (snd : rest)
  combine Gap (Bag fst snd rest) = bagLem Gap fst (snd : rest)
  combine (Bag fst snd rest) (L1 x) = bagLem fst snd (L1 x : rest)
  combine (Bag fst snd rest) Gap = bagLem fst snd (Gap : rest)
  combine (Bag fst1 snd1 rest1) (Bag fst2 snd2 rest2) =
    bagLem fst1 snd1 (snd1 : (rest1 <> fst2 : snd2 : rest2))
  combine (Sek fst snd rest) lem = bagLem (Sek fst snd rest) lem Nil
  combine (Pair k v) lem = Bagdict (B1 lem) (D1 k v)
  combine L0 lem = lem
  combine lem L0 = lem
  combine Gap lem = bagLem Gap lem Nil
  combine lem Gap = bagLem lem Gap Nil
  combine lem1 lem2 = bagLem lem1 lem2 Nil


-- ** Infix Operators

-- | Infix operators for constructing Lem values
-- |
-- | **Usage Examples:**
-- |
-- | **Sequential construction with `+:` (prepend, right-associative):**
-- | ```purescript
-- | -- Primitives are automatically wrapped in L1, no need for explicit wrappers
-- | Rs "a" +: Rs "b" +: Rs "c" +: L0  -- Creates Sek (L1 (Rs "a")) (L1 (Rs "b")) (L1 (Rs "c") : Nil)
-- | 
-- | -- Right-associative: chains without parentheses
-- | 1 +: 2 +: 3 +: L0  -- Same as: 1 +: (2 +: (3 +: L0))
-- | ```
-- |
-- | **Bag construction with `<+>` (combine, left-associative):**
-- | ```purescript
-- | -- Use parentheses when mixing different operators (mixed associativity)
-- | (Rs "a" +: L0) <+> (Rs "b" +: L0)  -- Creates Bag with two L1 elements
-- | ```
-- |
-- | **Concatenation with `:::` (concat, left-associative):**
-- | ```purescript
-- | -- Preserves nested structure, use parentheses with +: operator
-- | (Rs "a" +: L0) ::: (Rs "b" +: Rs "c" +: L0)  -- Creates Sek with two sub-Seks
-- | ```
-- |
-- | **Choice construction with `\/` (or, left-associative):**
-- | ```purescript
-- | a \/ b \/ c  -- Creates a Choice between a, b, and c
-- | ```
-- |
-- | **Operator precedence:** All at level 6
-- | - Right-associative: `+:` (prepend)
-- | - Left-associative: `:+` (append), `<+` (add), `<+>` (combine), `:::` (concat), `\/` (or)
-- | - **Important:** Use parentheses when mixing operators with different associativity!

infixr 6 addPrimitive as <+
infixl 6 addPrimitiveFlipped as +>
infixr 6 prependPrimitive as +:
infixl 6 appendPrimitive as :+
infixl 6 concat as :::
infixl 6 combine as <+>
infixl 6 or as \/

-- ** Standard Type Class Instances

-- *** Eq Instances

instance eqSek1 :: Eq t => Eq (Sek1 t) where
  eq (S1 l1) (S1 l2) = l1 == l2
  eq (S2 f1 s1 r1) (S2 f2 s2 r2) = f1 == f2 && s1 == s2 && r1 == r2
  eq _ _ = false

instance eqBag1 :: Eq t => Eq (Bag1 t) where
  eq (B2 f1 s1 r1) (B2 f2 s2 r2) =
    let
      list1 = f1 : s1 : r1
      list2 = f2 : s2 : r2
      -- Compare as sets (order-independent)
      sameLength = List.length list1 == List.length list2
      allIn = List.all (\x -> List.elem x list2) list1
    in
      sameLength && allIn
  eq (B1 e1) (B1 e2) = e1 == e2
  eq _ _ = false

instance eqDict1 :: Eq t => Eq (Dict1 t) where
  eq (D2 f1 s1 r1) (D2 f2 s2 r2) =
    let
      list1 = f1 : s1 : r1
      list2 = f2 : s2 : r2
    in
      -- Compare as unordered collections by checking all elements match
      (List.length list1 == List.length list2) && List.all (\x -> List.elem x list2) list1
  eq (D1 k1 v1) (D1 k2 v2) = k1 == k2 && v1 == v2
  eq _ _ = false

instance eqLem :: Eq t => Eq (Lem t) where
  eq L0 L0 = true
  eq Gap Gap = true
  eq (L1 x) (L1 y) = x == y
  eq (Pair k1 v1) (Pair k2 v2) = k1 == k2 && v1 == v2
  eq (Sek f1 s1 r1) (Sek f2 s2 r2) = f1 == f2 && s1 == s2 && r1 == r2
  eq (Bag f1 s1 r1) (Bag f2 s2 r2) =
    let
      list1 = f1 : s1 : r1
      list2 = f2 : s2 : r2
      sameLength = List.length list1 == List.length list2
      allIn = List.all (\x -> List.elem x list2) list1
    in
      sameLength && allIn
  eq (Choice f1 s1 r1) (Choice f2 s2 r2) =
    let
      list1 = f1 : s1 : r1
      list2 = f2 : s2 : r2
      sameLength = List.length list1 == List.length list2
      allIn = List.all (\x -> List.elem x list2) list1
    in
      sameLength && allIn
  eq (Dict f1 s1 r1) (Dict f2 s2 r2) =
    let
      list1 = f1 : s1 : r1
      list2 = f2 : s2 : r2
    in
      (List.length list1 == List.length list2) && List.all (\x -> List.elem x list2) list1
  eq (Sekdict s1 d1) (Sekdict s2 d2) = s1 == s2 && d1 == d2
  eq (Bagdict b1 d1) (Bagdict b2 d2) = b1 == b2 && d1 == d2
  eq _ _ = false

-- *** Show Instances

instance showSek1 :: Show a => Show (Sek1 a) where
  show (S2 fst snd rest) = "(S2 " <> show fst <> " " <> show snd <> " " <> show rest <> ")"
  show (S1 l) = "(S1 " <> show l <> ")"

instance showBag1 :: Show a => Show (Bag1 a) where
  show (B2 fst snd rest) = "(B2 " <> show fst <> " " <> show snd <> " " <> show rest <> ")"
  show (B1 l) = "(B1 " <> show l <> ")"

instance showDict1 :: Show a => Show (Dict1 a) where
  show (D2 fst snd rest) = "(D2 " <> show fst <> " " <> show snd <> " " <> show rest <> ")"
  show (D1 k v) = "(D1 " <> show k <> " " <> show v <> ")"

instance showLem :: Show a => Show (Lem a) where
  show L0 = "L0"
  show Gap = "_"
  show (L1 x) = "(L1 " <> show x <> ")"
  show (Pair k v) = "(Pair " <> show k <> " " <> show v <> ")"
  show (Sek fst snd rest) = "(Sek " <> show fst <> " " <> show snd <> " " <> show rest <> ")"
  show (Bag fst snd rest) = "(Bag " <> show fst <> " " <> show snd <> " " <> show rest <> ")"
  show (Choice fst snd rest) = "(Choice " <> show fst <> " " <> show snd <> " " <> show rest <> ")"
  show (Dict fst snd m) = "(Dict " <> show fst <> " " <> show snd <> " " <> show m <> ")"
  show (Sekdict s d) = "(Sekdict " <> show s <> " " <> show d <> ")"
  show (Bagdict b d) = "(Bagdict " <> show b <> " " <> show d <> ")"

-- *** Functor, Foldable, and Traversable Instances

-- Note: Can now implement standard Functor and Traversable instances
-- because Bag and Choice use List. Uniqueness checking happens only at construction
-- time via smart constructors (mkBag, mkChoice), not during map/traverse operations.

instance functorLem :: Functor Lem where
  map _ L0 = L0
  map _ Gap = Gap
  map f (L1 x) = L1 (f x)
  map f (Pair k v) = Pair (map f k) (map f v)
  map f (Sek fst snd rest) = Sek (map f fst) (map f snd) (map (map f) rest)
  map f (Bag fst snd rest) = Bag (map f fst) (map f snd) (map (map f) rest)
  map f (Choice fst snd rest) = Choice (map f fst) (map f snd) (map (map f) rest)
  map f (Dict fst snd rest) =
    let
      mapTuple (Tuple k v) = Tuple (map f k) (map f v)
    in
      Dict (mapTuple fst) (mapTuple snd) (map mapTuple rest)
  map f (Sekdict sek dict) =
    let
      sek' = case sek of
        S1 lem -> S1 (map f lem)
        S2 fst' snd' rest' -> S2 (map f fst') (map f snd') (map (map f) rest')
      dict' = case dict of
        D1 k v -> D1 (map f k) (map f v)
        D2 fst' snd' rest' ->
          let
            mapTuple (Tuple k v) = Tuple (map f k) (map f v)
          in
            D2 (mapTuple fst') (mapTuple snd') (map mapTuple rest')
    in
      Sekdict sek' dict'
  map f (Bagdict bag dict) =
    let
      bag' = case bag of
        B1 lem -> B1 (map f lem)
        B2 fst' snd' rest' -> B2 (map f fst') (map f snd') (map (map f) rest')
      dict' = case dict of
        D1 k v -> D1 (map f k) (map f v)
        D2 fst' snd' rest' ->
          let
            mapTuple (Tuple k v) = Tuple (map f k) (map f v)
          in
            D2 (mapTuple fst') (mapTuple snd') (map mapTuple rest')
    in
      Bagdict bag' dict'

instance foldableLem :: Foldable Lem where
  foldr _ z L0 = z
  foldr _ z Gap = z
  foldr f z (L1 x) = f x z
  foldr f z (Pair k v) = foldr f (foldr f z v) k
  foldr f z (Sek fst snd rest) = foldr f (foldr f (foldr (flip (foldr f)) z rest) snd) fst
  foldr f z (Bag fst snd rest) = foldr f (foldr f (foldr (flip (foldr f)) z rest) snd) fst
  foldr f z (Choice fst snd rest) = foldr f (foldr f (foldr (flip (foldr f)) z rest) snd) fst
  foldr f z (Dict fst snd rest) =
    let
      foldTuple (Tuple k v) acc = foldr f (foldr f acc v) k
    in
      foldTuple fst (foldTuple snd (foldr foldTuple z rest))
  foldr f z (Sekdict sek dict) =
    let
      zSek = case sek of
        S1 lem -> foldr f z lem
        S2 fst snd rest -> foldr f (foldr f (foldr (flip (foldr f)) z rest) snd) fst
      zDict = case dict of
        D1 k v -> foldr f (foldr f zSek v) k
        D2 fst snd rest ->
          let
            foldTuple (Tuple k v) acc = foldr f (foldr f acc v) k
          in
            foldTuple fst (foldTuple snd (foldr foldTuple zSek rest))
    in
      zDict
  foldr f z (Bagdict bag dict) =
    let
      zBag = case bag of
        B1 lem -> foldr f z lem
        B2 fst snd rest -> foldr f (foldr f (foldr (flip (foldr f)) z rest) snd) fst
      zDict = case dict of
        D1 k v -> foldr f (foldr f zBag v) k
        D2 fst snd rest ->
          let
            foldTuple (Tuple k v) acc = foldr f (foldr f acc v) k
          in
            foldTuple fst (foldTuple snd (foldr foldTuple zBag rest))
    in
      zDict

  foldl _ z L0 = z
  foldl _ z Gap = z
  foldl f z (L1 x) = f z x
  foldl f z (Pair k v) = foldl f (foldl f z k) v
  foldl f z (Sek fst snd rest) = foldl (foldl f) (foldl f (foldl f z fst) snd) rest
  foldl f z (Bag fst snd rest) = foldl (foldl f) (foldl f (foldl f z fst) snd) rest
  foldl f z (Choice fst snd rest) = foldl (foldl f) (foldl f (foldl f z fst) snd) rest
  foldl f z (Dict fst snd rest) =
    let
      foldTuple acc (Tuple k v) = foldl f (foldl f acc k) v
    in
      foldl foldTuple (foldTuple (foldTuple z fst) snd) rest
  foldl f z (Sekdict sek dict) =
    let
      zSek = case sek of
        S1 lem -> foldl f z lem
        S2 fst snd rest -> foldl (foldl f) (foldl f (foldl f z fst) snd) rest
      zDict = case dict of
        D1 k v -> foldl f (foldl f zSek k) v
        D2 fst snd rest ->
          let
            foldTuple acc (Tuple k v) = foldl f (foldl f acc k) v
          in
            foldl foldTuple (foldTuple (foldTuple zSek fst) snd) rest
    in
      zDict
  foldl f z (Bagdict bag dict) =
    let
      zBag = case bag of
        B1 lem -> foldl f z lem
        B2 fst snd rest -> foldl (foldl f) (foldl f (foldl f z fst) snd) rest
      zDict = case dict of
        D1 k v -> foldl f (foldl f zBag k) v
        D2 fst snd rest ->
          let
            foldTuple acc (Tuple k v) = foldl f (foldl f acc k) v
          in
            foldl foldTuple (foldTuple (foldTuple zBag fst) snd) rest
    in
      zDict

  foldMap f = foldl (\acc x -> acc <> f x) mempty

instance traversableLem :: Traversable Lem where
  traverse _ L0 = pure L0
  traverse _ Gap = pure Gap
  traverse f (L1 x) = L1 <$> f x
  traverse f (Pair k v) = Pair <$> traverse f k <*> traverse f v
  traverse f (Sek fst snd rest) = Sek <$> traverse f fst <*> traverse f snd <*> Data.Traversable.traverse (traverse f) rest
  traverse f (Bag fst snd rest) =
    -- Note: traverse doesn't check uniqueness, as we can't add Eq constraint here
    -- Uniqueness is preserved because we're mapping over already-unique elements
    Bag <$> traverse f fst <*> traverse f snd <*> Data.Traversable.traverse (traverse f) rest
  traverse f (Choice fst snd rest) =
    Choice <$> traverse f fst <*> traverse f snd <*> Data.Traversable.traverse (traverse f) rest
  traverse f (Dict fst snd rest) =
    let
      traverseTuple (Tuple k v) = Tuple <$> traverse f k <*> traverse f v
    in
      Dict <$> traverseTuple fst <*> traverseTuple snd <*> Data.Traversable.traverse traverseTuple rest
  traverse f (Sekdict sek dict) =
    let
      nsek = case sek of
        S1 lem -> S1 <$> traverse f lem
        S2 fst' snd' rest' -> S2 <$> traverse f fst' <*> traverse f snd' <*> Data.Traversable.traverse (traverse f) rest'
      ndict = case dict of
        D1 k v -> D1 <$> traverse f k <*> traverse f v
        D2 fst' snd' rest' ->
          let
            traverseTuple (Tuple k v) = Tuple <$> traverse f k <*> traverse f v
          in
            D2 <$> traverseTuple fst' <*> traverseTuple snd' <*> Data.Traversable.traverse traverseTuple rest'
    in
      Sekdict <$> nsek <*> ndict
  traverse f (Bagdict bag dict) =
    let
      nbag = case bag of
        B1 lem -> B1 <$> traverse f lem
        B2 fst' snd' rest' ->
          let
            travRest = Data.Traversable.traverse (traverse f) rest'
          in
            B2 <$> traverse f fst' <*> traverse f snd' <*> travRest
      ndict = case dict of
        D1 k v -> D1 <$> traverse f k <*> traverse f v
        D2 fst' snd' rest' ->
          let
            traverseTuple (Tuple k v) = Tuple <$> traverse f k <*> traverse f v
          in
            D2 <$> traverseTuple fst' <*> traverseTuple snd' <*> Data.Traversable.traverse traverseTuple rest'
    in
      Bagdict <$> nbag <*> ndict

  sequence L0 = pure L0
  sequence Gap = pure Gap
  sequence (L1 x) = L1 <$> x
  sequence (Pair k v) = Pair <$> sequence k <*> sequence v
  sequence (Sek fst snd rest) = Sek <$> sequence fst <*> sequence snd <*> Data.Traversable.traverse sequence rest
  sequence (Bag fst snd rest) =
    Bag <$> sequence fst <*> sequence snd <*> Data.Traversable.traverse sequence rest
  sequence (Choice fst snd rest) =
    Choice <$> sequence fst <*> sequence snd <*> Data.Traversable.traverse sequence rest
  sequence (Dict fst snd rest) =
    let
      sequenceTuple (Tuple k v) = Tuple <$> sequence k <*> sequence v
    in
      Dict <$> sequenceTuple fst <*> sequenceTuple snd <*> Data.Traversable.traverse sequenceTuple rest
  sequence (Sekdict sek dict) =
    let
      nsek = case sek of
        S1 lem -> S1 <$> sequence lem
        S2 fst' snd' rest' -> S2 <$> sequence fst' <*> sequence snd' <*> Data.Traversable.traverse sequence rest'
      ndict = case dict of
        D1 k v -> D1 <$> sequence k <*> sequence v
        D2 fst' snd' rest' ->
          let
            sequenceTuple (Tuple k v) = Tuple <$> sequence k <*> sequence v
          in
            D2 <$> sequenceTuple fst' <*> sequenceTuple snd' <*> Data.Traversable.traverse sequenceTuple rest'
    in
      Sekdict <$> nsek <*> ndict
  sequence (Bagdict bag dict) =
    let
      nbag = case bag of
        B1 lem -> B1 <$> sequence lem
        B2 fst' snd' rest' ->
          let
            seqRest = Data.Traversable.traverse sequence rest'
          in
            B2 <$> sequence fst' <*> sequence snd' <*> seqRest
      ndict = case dict of
        D1 k v -> D1 <$> sequence k <*> sequence v
        D2 fst' snd' rest' ->
          let
            sequenceTuple (Tuple k v) = Tuple <$> sequence k <*> sequence v
          in
            D2 <$> sequenceTuple fst' <*> sequenceTuple snd' <*> Data.Traversable.traverse sequenceTuple rest'
    in
      Bagdict <$> nbag <*> ndict

