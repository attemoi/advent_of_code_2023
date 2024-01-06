module Main where

import Prelude
import Data.Array.NonEmpty (toArray)
import Data.CodePoint.Unicode (isDecDigit)
import Data.Either (hush)
import Data.Foldable (sum)
import Data.Int (fromString)
import Data.List (List(..), any, catMaybes, filter, mapMaybe, (:))
import Data.List as List
import Data.List.NonEmpty (toList)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (CodePoint, codePointFromChar, length, singleton)
import Data.String.CodeUnits (fromCharArray)
import Effect (Effect)
import Effect.Console (log)
import Parsing (Parser, Position(..), position)
import Parsing.Combinators.Array (many1)
import Parsing.String (satisfyCodePoint)
import Parsing.String.Basic (digit)
import Parsing.String.Replace (splitCap)
import Utils (readFile)

type EnginePart
  = { col :: Int, row :: Int, num :: Int, length :: Int }

type EngineSymbol
  = { col :: Int, row :: Int, symbol :: String }

main :: Effect Unit
main = do
  input <- readFile "03-gear-ratios/input.txt"
  let
    partNumbers = catMaybes $ toList $ hush <$> splitCap input enginePartParser

    symbols = catMaybes $ toList $ hush <$> splitCap input symbolParser
  log $ show $ "Part1, sum of part nums: "
    <> (show $ sum $ map _.num $ filter (\part -> isAdjacentToAnySymbol part symbols) partNumbers)
  log $ show $ "Part2, sum of gear ratios: "
    <> (show $ sum $ mapMaybe (getGearRatio partNumbers) symbols)

enginePartParser :: Parser String EnginePart
enginePartParser = do
  pos <- position
  partNo <- many1Digit
  pure { col: column pos, row: row pos, num: partNo, length: length $ show $ partNo }

many1Digit :: Parser String Int
many1Digit = do
  digits <- many1 digit
  pure $ fromMaybe 0 (fromString $ fromCharArray (toArray digits))

symbolParser :: Parser String EngineSymbol
symbolParser = do
  pos <- position
  cp <- satisfyCodePoint isSymbol
  pure { col: column pos, row: row pos, symbol: singleton cp }

column :: Position -> Int
column (Position a) = _.column a

row :: Position -> Int
row (Position a) = _.line a

isAdjacentToAnySymbol :: EnginePart -> List EngineSymbol -> Boolean
isAdjacentToAnySymbol part symbols = any (isAdjacentToSymbol part) symbols

findAdjacentParts :: EngineSymbol -> List EnginePart -> List EnginePart
findAdjacentParts symbol parts = filter (\part -> isAdjacentToSymbol part symbol) parts

getGearRatio :: List EnginePart -> EngineSymbol -> Maybe Int
getGearRatio parts symbol
  | symbol.symbol == "*" = case findAdjacentParts symbol parts of
    (a : b : Nil) -> Just (a.num * b.num)
    _ -> Nothing

getGearRatio _ _ = Nothing

isAdjacentToExactlyTwoParts :: EngineSymbol -> List EnginePart -> Boolean
isAdjacentToExactlyTwoParts symbol parts = 2 == List.length (filter (\part -> isAdjacentToSymbol part symbol) parts)

isAdjacentToSymbol :: EnginePart -> EngineSymbol -> Boolean
isAdjacentToSymbol part symbol =
  (symbol.col >= part.col - 1 && symbol.col <= part.col + part.length)
    && (symbol.row >= part.row - 1 && symbol.row <= part.row + 1)

isPeriod :: CodePoint -> Boolean
isPeriod cp = cp == codePointFromChar '.'

isNewLine :: CodePoint -> Boolean
isNewLine cp = cp == codePointFromChar '\n'

isSymbol :: CodePoint → Boolean
isSymbol = (not isPeriod) && (not isDecDigit) && (not isNewLine)
