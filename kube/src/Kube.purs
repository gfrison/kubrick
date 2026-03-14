module Kubrick.Kube
  ( (+)
  , (+*)
  , (+>)
  , add
  , addAll
  , addFrom
  , addM
  , get
  , match
  , module Kubrick.Kube.Types
  , fill
  )
  where

import Prelude

import Control.Monad.State (State, runState)
import Data.Array as Array
import Data.Either (Either)
import Data.Foldable (class Foldable)
import Data.List.Lazy (List)
import Data.Maybe (Maybe, fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\), type (/\))
import Kubrick.Builder as Builder
import Kubrick.Getter as Getter
import Kubrick.Kube.Types (Kid(..), Bi, Kube, bi0, emptyKube)
import Kubrick.Lem (Lem)
import Kubrick.Matcher as Matcher
import Kubrick.Reticolo (Reticolo)
import Kubrick.Filler as Filler
import Kubrick.Types (Raw, Vid, Term)

-- | Add a Lem to a Kube starting with Kid 0
add :: forall a. Ord a => Kube a -> Lem a -> (Kube a /\ Kid)
add kube lem = addFrom (kube /\ Kid 0) lem

-- | Add a Lem to a Kube starting from a specific Kid
addFrom :: forall a. Ord a => (Kube a /\ Kid) -> Lem a -> (Kube a /\ Kid)
addFrom (kube /\ startKid) lem = 
  let Tuple insertedKid (Tuple _ newKube) = runState (Builder.add lem) (Tuple startKid kube)
  in newKube /\ insertedKid

-- | Add multiple Lems to a Kube, returns the Kube and the last Kid added
addAll :: forall f a. Foldable f => Ord a => Kube a -> f (Lem a) -> (Kube a /\ Kid)
addAll kube lems = 
  let lemsArray = Array.fromFoldable lems
      Tuple lastKid (Tuple _ finalKube) = runState 
        (do
          kids <- traverse (\lem -> addM lem) lemsArray
          pure $ fromMaybe (Kid 0) (Array.last kids)
        )
        (Tuple (Kid 0) kube)
  in finalKube /\ lastKid

-- | State monad version of add (original Builder.add)
addM :: forall a. Ord a => Lem a -> State (Tuple Kid (Kube a)) Kid
addM = Builder.add

infix 6 add as +
infix 6 addFrom as +>
infix 6 addAll as +*

get :: forall a. Eq a => Ord a => Kube a -> Kid -> Maybe (Lem a)
get = Getter.get 

match :: forall a. Ord a => Kube a -> Lem a -> List Kid 
match = Matcher.match

fill :: Kube Raw -> Kid -> Lem Term -> Either String (Reticolo Vid)
fill = Filler.fill