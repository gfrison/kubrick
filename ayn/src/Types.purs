module Types where

import Prelude

import Data.List (List(..), (:))
import Data.String as String
import Kubrick.Lem (Lem)
import Kubrick.Types (Raw, Atom)
import Kubrick.Kube.Types (Kube)

type ParseResult = { facts :: List (Lem Raw) }

data Method
  = Fun { head :: Lem Atom, body :: Lem Atom }
  | Impl { head :: Lem Atom, body :: Lem Atom }

instance eqMethod :: Eq Atom => Eq Method where
  eq (Fun r1) (Fun r2) = r1.head == r2.head && r1.body == r2.body
  eq (Impl r1) (Impl r2) = r1.head == r2.head && r1.body == r2.body
  eq _ _ = false

instance showMethod :: Show Atom => Show Method where
  show (Fun r) = show r.head <> " =| " <> show r.body
  show (Impl r) = show r.head <> " -| " <> show r.body

newtype Program = Program { facts :: List (Lem Raw), queries :: List (Lem Atom), methods :: List Method }
newtype Bundles = Bundles { facts :: Kube Raw, methods :: Kube Raw, queries :: List (Lem Atom) }

derive instance eqProgram :: Eq Program

instance showProgram :: Show Program where
  show (Program p) =
    let
      factLines = map show p.facts
      queryLines = map (\q -> "?" <> show q) p.queries
      methodLines = map show p.methods
      allLines = factLines <> queryLines <> methodLines
    in
      String.joinWith "\n" $ map identity $ toArray allLines
    where
    toArray :: List String -> Array String
    toArray Nil = []
    toArray (x : xs) = [ x ] <> toArray xs