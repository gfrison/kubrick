module Kubrick.Matcher(match) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.List as List
import Data.List.Lazy (List, Step(..), step, iterate, take, toUnfoldable, nil)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Set as Set
import Data.Tuple.Nested ((/\), type (/\))
import Kubrick.Kube.Types (Kid(..), Bi, Kube, getKeys)
import Kubrick.Lem (Lem(..), (<+>), (<+))
import Type.Data.Boolean (class Or)

{-
  return all Kids in the Kube that match the given Lem
  the match is partial and not strict. 
  # input sek
  input Sek matches with other sequences in Kube if the input represents their initial subsequence (no gaps).
  ex: Sek 1 2 matches with Sek 1 2 3 but not Sek 0 1 2 3 nor Sek 1 3 2
  # input Bag
  input Bag matches with stored set (Bag/Choice/Bagdict/Choicedict) if all the input elements are contained

  L1 matches with Lem stored in Kube that are:
  - L1 with same value
  - Sek which head is that L1 or a Bag/Choice containing that L1 (regardless other elements)
  - Bag/dict or Choice/dict containing that L1 in their set. Their dict won't be considered.
  - No Pairs, No Dict
  Pair matches with
  - Pair with same key and value
  - Dict containing that key and value
  - Sekdict/Bagdict/Choicedict containing that key and value 
  - nothing else
  example:
  L1 3 -> Sek 3 4 5 / Bag 1 2 3 / Choice 3 4 5
  Sek 3 4 -> Sek 3 4 5 / Sekdict containing Sek 3 4
  Sek 3 4 5 can't match with Sek 3 4
  Bag 1 2 -> Bag 1 2 3 / Bagdict containing Bag
  Choice 1 2 -> Choice 1 2 3 / Choicedict containing Choice
  Pair k v -> Pair k v / Dict containing Pair k v / Sekdict/Bagdict/Choicedict containing Pair k v
  all the input Lem matches where all their elements matches, and their elements can be seen related with 
  an AND logic operator.
  Differently, Choice's input element are related with an OR logic operator.
  so a Choice(Sek(1 2), Sek(3 4)) matches with any Kid that matches with Sek(1 2) or Sek(3 4)
-} 

match :: forall a. Ord a => Eq a => Lem a /\ Kube a -> List Kid
match (lem /\ kube) = nil