module Kubrick.Types
  ( Var(..)
  , Vid(..)
  , RawType(..)
  , Raw(..)
  , Term(..)
  , Atom(..)
  ) where

import Prelude

import Data.Hashable (class Hashable, hash)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
import Data.String.CodeUnits as SCU

data Var = FreeVar | Sar String
newtype Vid = Vid Int

derive instance eqVid :: Eq Vid
derive instance ordVid :: Ord Vid

instance showVid :: Show Vid where
  show (Vid n) = "Vid " <> show n

instance hashableVid :: Hashable Vid where
  hash (Vid n) = hash n

derive instance eqVar :: Eq Var
derive instance ordVar :: Ord Var

instance Hashable Var where
  hash FreeVar = hash (0 :: Int)
  hash (Sar s) = hash s

instance showVar :: Show Var where
  show FreeVar = "_"
  show (Sar s) = s

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
data Atom = Ar Raw | Av Var

derive instance eqAtom :: Eq Atom
derive instance ordAtom :: Ord Atom

instance showAtom :: Show Atom where
  show (Ar r) = show r
  show (Av v) = show v

instance hashableAtom :: Hashable Atom where
  hash (Ar r) = hash r
  hash (Av v) = hash v

derive instance eqRaw :: Eq Raw
derive instance ordRaw :: Ord Raw

instance showRaw :: Show Raw where
  show (Ri n) = show n
  show (Rf n) = show n
  show (Rs s) = showRawString s
  show (Rb b) = show b

showRawString :: String -> String
showRawString s =
  let
    startsWithUpperOrDigit = case SCU.charAt 0 s of
      Just c -> (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')
      Nothing -> false
    hasSpecial =
      contains (Pattern " ") s
      || contains (Pattern ";") s
      || contains (Pattern ",") s
      || contains (Pattern "{") s
      || contains (Pattern "}") s
      || contains (Pattern "[") s
      || contains (Pattern "]") s
      || contains (Pattern "(") s
      || contains (Pattern ")") s
      || contains (Pattern "\"") s
      || contains (Pattern "->") s
      || contains (Pattern "=|") s
      || contains (Pattern "-|") s
      || contains (Pattern "?") s
      || contains (Pattern "\n") s
    mustQuote =
      SCU.length s == 0
      || s == "true" || s == "false"
      || s == "_"
      || startsWithUpperOrDigit
      || hasSpecial
  in
    if mustQuote then "\"" <> s <> "\"" else s

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