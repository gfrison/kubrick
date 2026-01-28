module Kubrick.Kube
  ( add
  , get
  , match
  , module Kubrick.Kube.Types
  )
  where

import Prelude

import Control.Monad.State (State)
import Data.List.Lazy (List)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Data.Tuple.Nested (type (/\))
import Kubrick.Builder as Builder
import Kubrick.Getter as Getter
import Kubrick.Kube.Types (Kid(..), Bi, Kube, bi0, emptyKube)
import Kubrick.Lem (Lem)
import Kubrick.Matcher as Matcher 

add :: forall t. Ord t => Eq t => Show t => Lem t -> State (Tuple Kid (Kube t)) Kid
add = Builder.add

get :: forall a. Ord a => Eq a => (Kid /\ Kube a) -> Maybe (Lem a)
get = Getter.get 

match :: forall a. Ord a => Eq a => (Lem a /\ Kube a) -> List Kid 
match = Matcher.match