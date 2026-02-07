module Kubrick.Types
  ( Var(..)
  , Vid(..)
  , RawType(..)
  , Raw(..)
  , Term(..)
  ) where

import Prelude

import Data.Hashable (class Hashable, hash)

data Var = FreeVar | Sar String
newtype Vid = Vid Int

derive instance eqVid :: Eq Vid
derive instance ordVid :: Ord Vid

instance showVid :: Show Vid where
  show (Vid n) = "Vid " <> show n

instance hashableVid :: Hashable Vid where
  hash (Vid n) = hash n

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

-- | Term can be either a variable ID or a raw value
data Term
  = TVid Vid
  | TRaw Raw

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

derive instance eqTerm :: Eq Term
derive instance ordTerm :: Ord Term

instance showTerm :: Show Term where
  show (TVid v) = "TermVid (" <> show v <> ")"
  show (TRaw r) = "TermRaw (" <> show r <> ")"

instance hashableTerm :: Hashable Term where
  hash (TVid v) = hash v
  hash (TRaw r) = hash r