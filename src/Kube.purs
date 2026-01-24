module Kube where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Set (Set, singleton, union, empty)
import Data.Map (Map, lookup, insertWith)
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