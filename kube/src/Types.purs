module Kubrick.Kube.Types
  ( Kid(..)
  , Bi
  , Kube
  , M2m(..)
  , put
  , getValues
  , getKeys
  , bi0
  , emptyKube
  ) where

import Prelude

import Data.Map (Map, lookup, insertWith)
import Data.Maybe (Maybe(..))
import Data.Set (Set, singleton, union, empty)
import Data.Set as Set

-- | M2m = Many-to-Many bidirectional map
data M2m k v = M2m (Map k (Set v)) (Map v (Set k))

put :: forall k v. Ord k => Ord v => k -> v -> M2m k v -> M2m k v
put k v (M2m kv vk) = M2m (insertWith union k (singleton v) kv) (insertWith union v (singleton k) vk)

getValues :: forall k v. Ord k => M2m k v -> k -> Set v
getValues (M2m kv _) k = case lookup k kv of
  Just vs -> vs
  Nothing -> empty

getKeys :: forall k v. Ord v => M2m k v -> v -> Set k
getKeys (M2m _ vk) v = case lookup v vk of
  Just ks -> ks
  Nothing -> empty

-- | Kid = identifier for documents
newtype Kid = Kid Int

derive instance eqKid :: Eq Kid
derive instance ordKid :: Ord Kid
derive newtype instance showKid :: Show Kid
derive newtype instance semiringKid :: Semiring Kid

-- | Bi = bidirectional map (M2m)
-- | Maps atoms (values/references) to Kids (document IDs)
type Bi a = M2m a Kid

-- | Empty bidirectional map
bi0 :: forall a. Ord a => Bi a
bi0 = M2m mempty mempty
{-
Kube record type
kid types are writted with "kn" where n is the kid number
M2m are written just with the k -> v mapping for clarity omitting the reverse mapping v -> k
L1 a       -> {seqs = [{a -> k0}], roots = {k0}}
Sek a b    -> {seqs = [{a -> k0}, {b -> k0}], roots = {k0}}
Bag a b    -> {keys = {a -> k0, b -> k0}], roots = {k0}, sets = {k0}}
Choice a b -> {keys = {a -> k0, b -> k0}, roots = {k0}}
Pair a b   -> {keys = {a -> k0}, vals = {b -> k0}, roots = {k0}}
Dict (ka /\ va) ( kb /\ vb) ->
  { keys    = [{ka -> k0}, {kb -> k1}]
  , vals    = [{va -> k0}, {vb -> k1}]
  , refKeys = {k2 -> {k0, k1}} 
  , roots   = {k2}
  }
(a +: b +: L0) + (c /\ d) -> {
  seqs = [{a -> k0}, {b -> k0}],
  keys = {c -> k1},
  vals = {d -> k1},
  refKeys = {k0 -> {k1}},
  roots = {k0}
}

Sek(Sek(a  b) Sek(c  d)) ->
  {
    seqs = [{a -> k0, c -> k1}, {b -> k0, d -> k1}],
    refSeqs = [{k0 -> k2}, {k1 -> k2}],
    roots = {k2}
  }

store 2 distinct Lem:
Sek a b, Sek 10 11 -> {seqs = [{a -> k0, 10 -> k1}, {b -> k0, 11 -> k1}], roots = {k0,k1}}
-}
type Kube a =
  { seqs :: Array (Bi a) -- Positional sequences (data values)
  , refSeqs :: Array (Bi Kid) -- Positional sequences (Kid references)
  , keys :: Bi a -- Key index (data values)
  , refKeys :: Bi Kid -- Key index (Kid references)
  , vals :: Bi a -- Value index (data values)
  , refVals :: Bi Kid -- Value index (Kid references)
  , roots :: Set Kid -- Top level document IDs
  , sets :: Set Kid -- Set IDs
  }

-- | Create empty kube
emptyKube :: forall a. Ord a => Kube a
emptyKube =
  { seqs: []
  , refSeqs: []
  , keys: bi0
  , refKeys: bi0
  , vals: bi0
  , refVals: bi0
  , roots: Set.empty
  , sets: Set.empty
  }
