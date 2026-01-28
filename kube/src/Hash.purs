module Kubrick.Hash
  ( KubrickHash
  , hash
  , hashCombine
  , hashInt
  , hashString
  , hashBoolean
  , hashArray
  , hashFromHashable
  ) where

import Prelude

import Data.Array as Array
import Data.Hashable as H

-- | A newtype wrapper for hash values
-- | We use a larger space and better distribution to avoid collisions with Kid values
newtype KubrickHash = KubrickHash Int

derive newtype instance eqKubrickHash :: Eq KubrickHash
derive newtype instance ordKubrickHash :: Ord KubrickHash
derive newtype instance showKubrickHash :: Show KubrickHash
derive newtype instance hashableKubrickHash :: H.Hashable KubrickHash

-- | Convert hash to Int for storage in Kube
hash :: KubrickHash -> Int
hash (KubrickHash h) = h

-- | Hash a value using the Hashable instance and apply better mixing
hashFromHashable :: forall a. H.Hashable a => a -> KubrickHash
hashFromHashable x = KubrickHash (finalizeMix32 (H.hash x))

-- | Hash an integer using a better distribution function
-- | Uses the MurmurHash3 finalizer mix to improve distribution
hashInt :: Int -> KubrickHash
hashInt x = KubrickHash (finalizeMix32 x)

-- | Hash a string using a polynomial rolling hash
-- | Similar to Java's String.hashCode() but with MurmurHash-style mixing
hashString :: String -> KubrickHash
hashString s = KubrickHash (stringHash s)

-- | Hash a boolean
hashBoolean :: Boolean -> KubrickHash
hashBoolean true = KubrickHash 1231  -- Common values from Java's Boolean.hashCode()
hashBoolean false = KubrickHash 1237

-- | Hash an array by combining element hashes
hashArray :: forall a. H.Hashable a => Array a -> KubrickHash
hashArray arr = 
  Array.foldl (\acc x -> hashCombine acc (KubrickHash (H.hash x))) (KubrickHash 0) arr

-- | Combine two hash values
-- | Uses a mixing function similar to Boost's hash_combine
-- | The golden ratio constant is -1640531527 in signed 32-bit representation
hashCombine :: KubrickHash -> KubrickHash -> KubrickHash
hashCombine (KubrickHash h1) (KubrickHash h2) = 
  KubrickHash (h1 `xor` (h2 + (-1640531527) + (h1 `shl` 6) + (h1 `shr` 2)))

-- | MurmurHash3's 32-bit finalizer
-- | This improves the distribution of hash values
finalizeMix32 :: Int -> Int
finalizeMix32 h0 = h3
  where
    h1 = h0 `xor` (h0 `zshr` 16)
    h2 = imul h1 (-2048144789)  -- 0x85ebca6b as signed 32-bit
    h3' = h2 `xor` (h2 `zshr` 13)
    h3'' = imul h3' (-1028477387)  -- 0xc2b2ae35 as signed 32-bit
    h3 = h3'' `xor` (h3'' `zshr` 16)

-- | String hash function (polynomial rolling hash with good multiplier)
-- | Similar to Java's String.hashCode()
foreign import stringHash :: String -> Int

-- Bit operations
foreign import shl :: Int -> Int -> Int
foreign import shr :: Int -> Int -> Int
foreign import zshr :: Int -> Int -> Int
foreign import xor :: Int -> Int -> Int
foreign import imul :: Int -> Int -> Int
