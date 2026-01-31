module Kubrick.Kube
  ( add
  , addFrom
  , addAll
  , addM
  , get
  , match
  , module Kubrick.Kube.Types, (+), (+>), (+*)
  )
  where

import Prelude

import Control.Monad.State (State, runState)
import Data.Foldable (class Foldable, foldl)
import Data.List.Lazy (List)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\), type (/\))
import Kubrick.Builder as Builder
import Kubrick.Getter as Getter
import Kubrick.Kube.Types (Kid(..), Bi, Kube, bi0, emptyKube)
import Kubrick.Lem (Lem)
import Kubrick.Matcher as Matcher
import Kubrick.Types (Raw)

-- | Add a Lem to a Kube starting with Kid 0
add :: Kube -> Lem Raw -> (Kube /\ Kid)
add kube lem = addFrom (kube /\ Kid 0) lem

-- | Add a Lem to a Kube starting from a specific Kid
addFrom :: (Kube /\ Kid) -> Lem Raw -> (Kube /\ Kid)
addFrom (kube /\ kid) lem = 
  let Tuple _ (Tuple nextKid newKube) = runState (Builder.add lem) (Tuple kid kube)
  in newKube /\ nextKid

-- | Add multiple Lems to a Kube, returns the Kube and the last Kid
addAll :: forall f. Foldable f => Kube -> f (Lem Raw) -> (Kube /\ Kid)
addAll kube lems = foldl (\(k /\ kid) lem -> addFrom (k /\ kid) lem) (kube /\ Kid 0) lems

-- | State monad version of add (original Builder.add)
addM :: Lem Raw -> State (Tuple Kid Kube) Kid
addM = Builder.add

infix 6 add as +
infix 6 addFrom as +>
infix 6 addAll as +*

get :: Kube -> Kid -> Maybe (Lem Raw)
get = Getter.get 

match :: Kube -> Lem Raw -> List Kid 
match = Matcher.match