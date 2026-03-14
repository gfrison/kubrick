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
