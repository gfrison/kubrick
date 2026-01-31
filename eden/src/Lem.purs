module Kubrick.Lem
  ( -- * Operators (recommended API - USE THESE)
    (<+)
  , (<+>)
  --|  prepend a primitive or Tuple to a Lem to another Lem
  --| 1 +: L1 2 -> Sek (L1 1) (L1 2) Nil
  --| (1 /\ 2) +: L1 3 -> Sekdict (S1 (L1 3)) (D1 (L1 1) (L1 2))
  --| (Sek (L1 2) (L1 3) Nil) +: L1 1 -> Sek (L1 1) (L1 2) (L1 3 : Nil)
  --| (Sek (L1 1) (L1 2) Nil) +: (Sek (L1 3) (L1 4) Nil) -> Sek (L1 1) (L1 2) (L1 3 : L1 4 : Nil)
  , (+:) 
  , (:+)
  --| creates a Sek indipendently of the arguments
  --| (Sek (L1 1) (L1 2) Nil) ::: (Sek (L1 3) (L1 4) Nil) -> Sek (Sek (L1 1) (L1 2) Nil) (Sek (L1 3) (L1 4) Nil) Nil
  , (:::)
  --| create a choice between two Lem values or a primitive and a Lem 
  , (\/)
  , class Or
  , or
  -- * Types
  -- | Note: Sekdict and Bagdict are created by operators, not by users directly
  , Lem(..)
  -- * Type Classes
  , class AddPrimitive
  , addPrimitive
  , class CombineLem
  , combine
  , class PreLem
  , concat
  , class PrePrimitive
  , prependPrimitive
  , class PostPrimitive
  , appendPrimitive 
  -- * Polymorphic Constructor
  , class MakeLem
  , lem
  -- * INTERNAL - Do not use directly (exported for Builder access only)
  , Sek1(..)
  , Bag1(..)
  , Dict1(..)
  ) where

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
    allElems = fst : snd : rest
    unique = List.nubByEq (==) allElems
  in
    case unique of
      f : s : r -> Choice f s r
      f : Nil -> f
      _ -> L0

-- Smart constructor for Choice that ensures uniqueness
-- | Takes primitive values and wraps them in L1
choice :: forall t. Eq t => t -> t -> List t -> Lem t
choice fst snd rest = choiceLem (L1 fst) (L1 snd) (map L1 rest)

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

-- | Smart constructor for D2 that ensures key uniqueness
-- | Takes tuples of primitive values and wraps them
d2 :: forall t. Eq t => Tuple t t -> Tuple t t -> List (Tuple t t) -> Dict1 t
d2 fst snd rest =
  let
    wrapPair (Tuple k v) = Tuple (L1 k) (L1 v)
  in
    d2Lem (wrapPair fst) (wrapPair snd) (map wrapPair rest)

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
  prependPrimitive lem (Pair k v) = Sekdict (S1 lem) (D1 k v)
  prependPrimitive lem (Dict fst snd rest) = Sekdict (S1 lem) (D2 fst snd rest)
  prependPrimitive L0 b = b
  prependPrimitive a L0 = a
  prependPrimitive (Pair a b) lem = (Pair a b) <+> lem
  prependPrimitive a b = Sek a b Nil
  
class PostPrimitive t a where
  appendPrimitive :: Eq t => Lem t -> a -> Lem t

instance postPrimitiveValue :: Eq t => PostPrimitive t t where
  appendPrimitive lem el = prependPrimitive el lem
else instance postTuple :: Eq t => PostPrimitive t (Tuple t t) where
  appendPrimitive lem (a /\ b) = prependPrimitive (a /\ b) lem
else instance appendLem :: Eq t => PostPrimitive t (Lem t) where
  appendPrimitive a b = prependPrimitive b a

-- Type class for addPrimitiveing an element to Lem, it will create Bag or Bagdict as needed
class AddPrimitive t a where
  addPrimitive :: Eq t => Lem t -> a -> Lem t

instance addPrimitiveValue :: Eq t => AddPrimitive t t where
  addPrimitive (L1 x) el = Sek (L1 x) (L1 el) Nil
  addPrimitive (Sek fst snd rest) el = Sek fst snd (List.snoc rest (L1 el))
  addPrimitive (Bag fst snd rest) el = bagLem fst snd (L1 el : rest)
  addPrimitive (Choice fst snd rest) el = choiceLem fst snd (L1 el : rest)
  addPrimitive lem el = combine lem (L1 el)
else instance addPrimitiveTuple :: Eq t => AddPrimitive t (Tuple t t) where
  addPrimitive (Pair k v) (a /\ b) = dictLem (Tuple k v) (Tuple (L1 a) (L1 b)) Nil
  addPrimitive (Dict fst snd rest) (a /\ b) = dictLem fst snd (Tuple (L1 a) (L1 b) : rest)
  addPrimitive lem (a /\ b) = combine lem (Pair (L1 a) (L1 b))

-- | Type class for combining two Lem values
class CombineLem t where
  combine :: Eq t => Lem t -> Lem t -> Lem t

instance combineLem :: Eq t => CombineLem t where
  combine (L1 x) (L1 y) = bagLem (L1 x) (L1 y) Nil
  combine (L1 x) (Bag fst snd rest) = bagLem (L1 x) fst (snd : rest)
  combine (Bag fst1 snd1 rest1) (Bag fst2 snd2 rest2) =
    bagLem fst1 snd1 (snd1 : (rest1 <> fst2 : snd2 : rest2))
  combine (Sek fst snd rest) lem = bagLem (Sek fst snd rest) lem Nil
  combine (Pair k v) lem = Bagdict (B1 lem) (D1 k v)
  combine L0 lem = lem
  combine lem L0 = lem
  combine lem1 lem2 = bagLem lem1 lem2 Nil

class Or s t where
  or :: Eq t => s -> Lem t -> Lem t
instance orLem :: Eq t => Or (Lem t) t where
  or lem L0  = lem
  or a b = choiceLem a b Nil
else instance orPrimitive :: Eq t => Or t t where
  or p lem = choiceLem (L1 p) lem Nil
-- ** Infix Operators

infixl 6 addPrimitive as <+
infixl 6 prependPrimitive as +:
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

