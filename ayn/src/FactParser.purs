module FactParser 
  ( parse
  , parseMultiline
  , tokenize
  , parseLemWith
  , parseRaw
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Int as Int
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.Traversable (traverse)
import Kubrick.Lem (Lem(..))
import Kubrick.Types (Raw(..))
import Types (ParseResult)

-- | Parse a string input
-- | Overall Complexity: O(n) where n is the total length of the input
-- |   - Multiline parsing: O(lines)
-- |   - Tokenization: O(n) where n is input length
-- |   - Parsing: O(tokens) where tokens ≤ n
-- | Total: O(n) - optimal for this problem
parse :: String -> Either String ParseResult
parse input = do
  lines <- Right $ parseMultiline input
  facts <- parseFacts lines
  pure { facts: facts }

-- | Parse multiline input with indentation support
-- | Returns a list of logical lines (where indented lines are appended to previous line)
-- | Complexity: O(n) where n is the number of lines
parseMultiline :: String -> List String
parseMultiline input =
  let
    lines = String.split (Pattern "\n") input
    -- Filter empty lines in one pass and convert to List
    nonEmptyLines = List.filter (not <<< isOnlyWhitespace) $ List.fromFoldable lines
  in
    foldLines nonEmptyLines Nil
  where
  isOnlyWhitespace :: String -> Boolean
  isOnlyWhitespace s = String.trim s == ""
  
  isIndented :: String -> Boolean  
  isIndented s = String.take 2 s == "  "
  
  -- Fold lines, combining indented lines with previous line
  -- Tail-recursive for performance
  foldLines :: List String -> List String -> List String
  foldLines Nil acc = List.reverse acc
  foldLines (line : rest) acc
    | isIndented line = case acc of
        Nil -> foldLines rest (line : acc)
        (prev : accRest) ->
          let combined = prev <> " " <> String.trim line
          in foldLines rest (combined : accRest)
    | otherwise = foldLines rest (line : acc)

-- | Parse facts from logical lines
parseFacts :: List String -> Either String (List (Lem Raw))
parseFacts lines = traverse parseLem lines

-- | Parse a single Lem
parseLem :: String -> Either String (Lem Raw)
parseLem input = parseLemWith parseRaw (tokenize input)

-- | Parse a Lem from tokens using a custom atom parser
parseLemWith :: forall t. (String -> Either String (Lem t)) -> List String -> Either String (Lem t)
parseLemWith atomParser tokens = do
  result <- parseExpression tokens
  pure result.lem
  where
  parseExpression :: List String -> Either String { lem :: Lem t, rest :: List String }
  parseExpression toks = parseSequence toks Nil
  
  constructFromAcc :: List (Lem t) -> (Lem t -> Lem t -> List (Lem t) -> Lem t) -> Either String (Lem t)
  constructFromAcc acc constructor =
    case List.reverse acc of
      Nil -> Right L0
      h : Nil -> Right h
      h : h2 : rest -> Right $ constructor h h2 rest
  
  parseSequence :: List String -> List (Lem t) -> Either String { lem :: Lem t, rest :: List String }
  parseSequence Nil acc = do
    value <- constructFromAcc acc Sek
    pure { lem: value, rest: Nil }
  parseSequence ts@(tok : rest) acc
    | tok == "," = do
        value <- constructFromAcc acc Sek
        pure { lem: value, rest: ts }
    | tok == "->" =
        case List.reverse acc of
          Nil -> Left "Empty before ->"
          key : Nil -> do
            valueResult <- parseSequence rest Nil
            pure { lem: Pair key valueResult.lem, rest: valueResult.rest }
          _ -> Left "Multiple elements before ->"
    | tok == ";" = do
        case acc of
          Nil -> Left "Empty before ;"
          prev : accRest -> do
            choiceResult <- parseChoiceAtTopLevel rest (prev : Nil)
            parseSequence choiceResult.rest (choiceResult.lem : accRest)
    | otherwise = do
        primResult <- parsePrimary ts
        parseSequence primResult.rest (primResult.lem : acc)
  
  parseChoiceAtTopLevel :: List String -> List (Lem t) -> Either String { lem :: Lem t, rest :: List String }
  parseChoiceAtTopLevel Nil _ = Left "Incomplete choice"
  parseChoiceAtTopLevel ts acc = do
    primResult <- parsePrimary ts
    case primResult.rest of
      (";") : restTokens -> parseChoiceAtTopLevel restTokens (primResult.lem : acc)
      _ -> do
        value <- constructFromAcc (primResult.lem : acc) Choice
        case value of
          Choice _ _ _ -> pure { lem: value, rest: primResult.rest }
          _ -> Left "Choice needs at least 2 elements"
  
  parsePrimary :: List String -> Either String { lem :: Lem t, rest :: List String }
  parsePrimary Nil = Left "Unexpected end"
  parsePrimary (tok : rest)
    | tok == "{" = parseDelimited "}" Bag rest
    | tok == "[" = parseDelimited "]" Sek rest
    | tok == "(" = parseDelimitedWithChoice ")" rest
    | tok == ";" = Left "Unexpected ;"
    | tok == ")" = Left "Unexpected )"
    | tok == "]" = Left "Unexpected ]"
    | tok == "}" = Left "Unexpected }"
    | otherwise = do
        lem <- atomParser tok
        pure { lem, rest }
  
  parseDelimited :: String -> (Lem t -> Lem t -> List (Lem t) -> Lem t) -> List String -> Either String { lem :: Lem t, rest :: List String }
  parseDelimited closingToken constructor = parseDelimitedContents Nil
    where
    parseDelimitedContents :: List (Lem t) -> List String -> Either String { lem :: Lem t, rest :: List String }
    parseDelimitedContents _ Nil = Left $ "Unclosed " <> closingToken
    parseDelimitedContents acc (tok : tokRest)
      | tok == closingToken = do
          lem <- constructFromAcc acc constructor
          pure { lem, rest: tokRest }
      | otherwise = do
          primResult <- parsePrimary (tok : tokRest)
          parseDelimitedContents (primResult.lem : acc) primResult.rest
  
  parseDelimitedWithChoice :: String -> List String -> Either String { lem :: Lem t, rest :: List String }
  parseDelimitedWithChoice closingToken = parseContents Nil
    where
    parseContents :: List (Lem t) -> List String -> Either String { lem :: Lem t, rest :: List String }
    parseContents _ Nil = Left "Unclosed ("
    parseContents acc ts@(tok : tokRest)
      | tok == closingToken = do
          value <- constructFromAcc acc Sek
          pure { lem: value, rest: tokRest }
      | tok == ";" = 
          case List.reverse acc of
            Nil -> Left "Empty before ;"
            current : Nil -> parseChoiceInParens tokRest (current : Nil)
            _ -> Left "Multiple elements before ; in choice"
      | otherwise = do
          primResult <- parsePrimary ts
          parseContents (primResult.lem : acc) primResult.rest
    
    parseChoiceInParens :: List String -> List (Lem t) -> Either String { lem :: Lem t, rest :: List String }
    parseChoiceInParens Nil _ = Left "Unclosed choice"
    parseChoiceInParens (tok : tokRest) acc
      | tok == closingToken = do
          lem <- constructFromAcc acc Choice
          case lem of
            Choice _ _ _ -> pure { lem, rest: tokRest }
            _ -> Left "Choice needs at least 2 elements"
      | tok == ";" = Left "Empty element in choice"
      | otherwise = do
          primResult <- parsePrimary (tok : tokRest)
          case primResult.rest of
            (";") : restTokens -> parseChoiceInParens restTokens (primResult.lem : acc)
            (")") : restTokens -> do
              lem <- constructFromAcc (primResult.lem : acc) Choice
              case lem of
                Choice _ _ _ -> pure { lem, rest: restTokens }
                _ -> Left "Choice needs at least 2 elements"
            _ -> Left "Expected ; or ) in choice"
tokenize :: String -> List String
tokenize input = 
  let
    trimmed = String.trim input
  in
    List.fromFoldable $ tokenizeImpl trimmed 0 "" [] false 0
  where
  -- Helper to flush current token to accumulator
  flushToken :: String -> Array String -> Array String
  flushToken current acc = if current == "" then acc else Array.snoc acc current

  -- Helper to add delimiter after flushing current token
  addDelimiter :: String -> String -> Array String -> Array String
  addDelimiter current delimiter acc = Array.snoc (flushToken current acc) delimiter

  tokenizeImpl :: String -> Int -> String -> Array String -> Boolean -> Int -> Array String
  tokenizeImpl str pos current acc inQuotes depth
    | pos >= SCU.length str = flushToken current acc
    | otherwise =
        let
          char = SCU.charAt pos str
          next = pos + 1
        in
          case char of
            Just '"' ->
              if inQuotes then
                tokenizeImpl str next "" (Array.snoc acc (current <> "\"")) false depth
              else
                tokenizeImpl str next "\"" acc true depth
            Just ' ' | not inQuotes ->
              tokenizeImpl str next "" (flushToken current acc) inQuotes depth
            Just ',' | not inQuotes && depth == 0 ->
              tokenizeImpl str next "" (addDelimiter current "," acc) inQuotes depth
            Just ';' | not inQuotes ->
              tokenizeImpl str next "" (addDelimiter current ";" acc) inQuotes depth
            Just '{' | not inQuotes ->
              tokenizeImpl str next "" (addDelimiter current "{" acc) false (depth + 1)
            Just '}' | not inQuotes ->
              tokenizeImpl str next "" (addDelimiter current "}" acc) false (depth - 1)
            Just '[' | not inQuotes ->
              tokenizeImpl str next "" (addDelimiter current "[" acc) false (depth + 1)
            Just ']' | not inQuotes ->
              tokenizeImpl str next "" (addDelimiter current "]" acc) false (depth - 1)
            Just '(' | not inQuotes ->
              tokenizeImpl str next "" (addDelimiter current "(" acc) false (depth + 1)
            Just ')' | not inQuotes ->
              tokenizeImpl str next "" (addDelimiter current ")" acc) false (depth - 1)
            Just '-' | not inQuotes && depth == 0 ->
              case SCU.charAt next str of
                Just '>' ->
                  tokenizeImpl str (next + 1) "" (addDelimiter current "->" acc) inQuotes depth
                _ -> tokenizeImpl str next (current <> "-") acc inQuotes depth
            Just c ->
              tokenizeImpl str next (current <> SCU.singleton c) acc inQuotes depth
            Nothing -> acc

-- | Parse raw values
parseRaw :: String -> Either String (Lem Raw)
parseRaw str
  | str == "true" = Right $ L1 $ Rb true
  | str == "false" = Right $ L1 $ Rb false
  | String.length str >= 2 && String.take 1 str == "\"" && String.drop (String.length str - 1) str == "\"" =
      Right $ L1 $ Rs (String.take (String.length str - 2) (String.drop 1 str))
  | otherwise =
      case Number.fromString str of
        Just n ->
          if String.contains (Pattern ".") str then
            Right $ L1 $ Rf n
          else
            case Int.fromString str of
              Just i -> Right $ L1 $ Ri i
              Nothing -> Right $ L1 $ Rs str
        Nothing -> Right $ L1 $ Rs str
