module Kubrick.Types
  ( Var(..)
  , Vid
  , RawType(..)
  , Raw(..)
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

-- | Raw cell type representation
data RawType = TInt | TFloat | TString | TBool

derive instance eqRawType :: Eq RawType
derive instance ordRawType :: Ord RawType

instance showRawType :: Show RawType where
  show TInt = "int"
  show TFloat = "float"
  show TString = "string"
  show TBool = "bool"

-- | Raw cell values
data Raw
  = Ri Int
  | Rf Number
  | Rs String
  | Rb Boolean
data Term = Var | Raw

derive instance eqRaw :: Eq Raw
derive instance ordRaw :: Ord Raw

instance showRaw :: Show Raw where
  show (Ri n) = show n
  show (Rf n) = show n
  show (Rs s) = show s
  show (Rb b) = show b

instance hashableRaw :: Hashable Raw where
  hash (Ri n) = hash n
  hash (Rf n) = hash n
  hash (Rs s) = hash s
  hash (Rb b) = hash b