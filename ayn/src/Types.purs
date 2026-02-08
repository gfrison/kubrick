module Types where
import Data.List (List)
import Kubrick.Lem (Lem)
import Kubrick.Types (Raw)
type ParseResult = {facts:: List (Lem Raw) }