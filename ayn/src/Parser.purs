module Parser
  ( parseMethod
  , parseProgram
  , parseAtom
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits as SCU
import FactParser (parseMultiline, tokenize, parseLemWith, parseRaw)
import Kubrick.Lem (Lem(..))
import Kubrick.Types (Atom(..), Raw, Var(..))
import Types (Method(..), Program(..))

-- | Parse an atom token: variables (capitalized words), _ (Gap), or raw values wrapped in Ar
parseAtom :: String -> Either String (Lem Atom)
parseAtom "_" = Right Gap
parseAtom str = case SCU.charAt 0 str of
  Just c | isUpperCase c && isAlphaNumeric str ->
    Right $ L1 $ Av $ Sar str
  _ -> map (map Ar) (parseRaw str)
  where
  isUpperCase :: Char -> Boolean
  isUpperCase c = c >= 'A' && c <= 'Z'

  isAlphaNumeric :: String -> Boolean
  isAlphaNumeric s = String.length s > 0 && allCharsAlphaNum s 0

  allCharsAlphaNum :: String -> Int -> Boolean
  allCharsAlphaNum s i
    | i >= SCU.length s = true
    | otherwise = case SCU.charAt i s of
        Just ch -> (isAlpha ch || isDigit ch) && allCharsAlphaNum s (i + 1)
        Nothing -> true

  isAlpha :: Char -> Boolean
  isAlpha c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')

  isDigit :: Char -> Boolean
  isDigit c = c >= '0' && c <= '9'

-- | Parse a method from a line: "head =| body" or "head -| body"
parseMethod :: String -> Either String Method
parseMethod input = case splitMethod input of
  Just { kind: "=|", headStr, bodyStr } -> do
    head <- parseLemWith parseAtom (tokenize headStr)
    body <- parseLemWith parseAtom (tokenize bodyStr)
    Right $ Fun { head, body }
  Just { kind: "-|", headStr, bodyStr } -> do
    head <- parseLemWith parseAtom (tokenize headStr)
    body <- parseLemWith parseAtom (tokenize bodyStr)
    Right $ Impl { head, body }
  _ -> Left "Not a method: missing =| or -| separator"
  where
  splitMethod :: String -> Maybe { kind :: String, headStr :: String, bodyStr :: String }
  splitMethod s = findSeparator s "=|" <|> findSeparator s "-|"

  findSeparator :: String -> String -> Maybe { kind :: String, headStr :: String, bodyStr :: String }
  findSeparator s sep = case String.indexOf (Pattern sep) s of
    Just idx ->
      let headStr = String.trim $ String.take idx s
          bodyStr = String.trim $ String.drop (idx + String.length sep) s
      in Just { kind: sep, headStr, bodyStr }
    Nothing -> Nothing

-- | Parse a full program from multiline input
parseProgram :: String -> Either String Program
parseProgram input = do
  let logicalLines = parseMultiline input
  classifyAndParse logicalLines
  where
  classifyAndParse :: List String -> Either String Program
  classifyAndParse lines = foldLines lines { facts: Nil, queries: Nil, methods: Nil }

  foldLines :: List String 
            -> { facts :: List (Lem Raw), queries :: List (Lem Atom), methods :: List Method }
            -> Either String Program
  foldLines Nil acc = Right $ Program
    { facts: List.reverse acc.facts
    , queries: List.reverse acc.queries
    , methods: List.reverse acc.methods
    }
  foldLines (line : rest) acc = do
    let trimmed = String.trim line
    if String.take 1 trimmed == "?" then do
      query <- parseLemWith parseAtom (tokenize (String.drop 1 trimmed))
      foldLines rest (acc { queries = query : acc.queries })
    else if isMethod trimmed then do
      method <- parseMethod trimmed
      foldLines rest (acc { methods = method : acc.methods })
    else do
      fact <- parseLemWith parseRaw (tokenize trimmed)
      validateNoVariables trimmed
      foldLines rest (acc { facts = fact : acc.facts })

  isMethod :: String -> Boolean
  isMethod s = String.contains (Pattern "=|") s || String.contains (Pattern "-|") s

  validateNoVariables :: String -> Either String Unit
  validateNoVariables s =
    let tokens = tokenize s
    in if List.any isVariable tokens
       then Left $ "Fact contains variables: " <> s
       else Right unit

  isVariable :: String -> Boolean
  isVariable tok = case SCU.charAt 0 tok of
    Just c -> c >= 'A' && c <= 'Z' && isAlphaNumeric tok
    Nothing -> false

  isAlphaNumeric :: String -> Boolean
  isAlphaNumeric s = String.length s > 0 && allCharsAlphaNum s 0

  allCharsAlphaNum :: String -> Int -> Boolean
  allCharsAlphaNum s i
    | i >= SCU.length s = true
    | otherwise = case SCU.charAt i s of
        Just ch -> (isAlpha ch || isDigit ch) && allCharsAlphaNum s (i + 1)
        Nothing -> true

  isAlpha :: Char -> Boolean
  isAlpha c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')

  isDigit :: Char -> Boolean
  isDigit c = c >= '0' && c <= '9'
