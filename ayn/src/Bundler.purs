module Bundler (bundle) where

import Prelude

import Data.Either (Either(..))
import Data.List (List, (:))
import Data.List as List
import Data.Tuple (fst)
import Kubrick.Kube (addAll)
import Kubrick.Kube.Types (Kube, emptyKube)
import Kubrick.Lem (Lem(..))
import Kubrick.Types (Atom(..), Raw(..))
import Types (Bundles(..), Method(..), Program(..))

-- | Convert a Program into Bundles by building Kubes for facts and methods
bundle :: Program -> Either String Bundles
bundle (Program p) = do
  let facts = buildKube p.facts
      methods = buildKube (map encodeMethod p.methods)
  Right $ Bundles { facts, methods, queries: p.queries }

-- | Build a Kube from a list of Lem Raw values
buildKube :: List (Lem Raw) -> Kube Raw
buildKube lems = fst (emptyKube `addAll` lems)

-- | Encode a Method as a Lem Raw Bag:
-- | Fun  { head, body } → Bag (type -> fun)  (head -> ...) (body -> ...)
-- | Impl { head, body } → Bag (type -> impl) (head -> ...) (body -> ...)
encodeMethod :: Method -> Lem Raw
encodeMethod (Fun { head, body }) = encodeParts "fun" head body
encodeMethod (Impl { head, body }) = encodeParts "impl" head body

encodeParts :: String -> Lem Atom -> Lem Atom -> Lem Raw
encodeParts kind head body =
  Bag
    (Pair (L1 (Rs "type")) (L1 (Rs kind)))
    (Pair (L1 (Rs "head")) (atomToRaw head))
    (Pair (L1 (Rs "body")) (atomToRaw body) : List.Nil)

-- | Convert a Lem Atom to Lem Raw by mapping Atom values to Raw
atomToRaw :: Lem Atom -> Lem Raw
atomToRaw = map atomValueToRaw

atomValueToRaw :: Atom -> Raw
atomValueToRaw (Ar raw) = raw
atomValueToRaw (Av var) = Rs (show var)
