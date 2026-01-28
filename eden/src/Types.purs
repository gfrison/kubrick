module Kubrick.Types
  ( Var(..)
  , Vid
  ) where

import Prelude

import Data.Hashable (class Hashable, hash)

data Var = FreeVar | Sar String
type Vid = Int

derive instance eqVar :: Eq Var
instance Hashable Var where
  hash FreeVar = hash (0 :: Int)
  hash (Sar s) = hash s

instance showVar :: Show Var where
  show FreeVar = "_"
  show (Sar s) = show s