module Main where

import Prelude

import Data.Array (uncons)
import Data.CodePoint.Unicode (isDecDigit)
import Data.Foldable (sum)
import Data.Int (fromString)
import Data.List (List(..), any, filter, mapMaybe, (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (CodePoint, codePointFromChar, drop, dropWhile, length, takeWhile)
import Effect (Effect)
import Effect.Console (log)
import Utils (readLines)

type EnginePart
  = { col :: Int, row :: Int, num :: Int, length :: Int }

type EngineSymbol
  = { col :: Int, row :: Int, symbol :: String }

type EngineSchematics
  = { parts :: List EnginePart, symbols :: List EngineSymbol }

type Gear
  = { a :: EnginePart, b :: EnginePart }

main :: Effect Unit
main = do
  lines <- readLines "03-gear-ratios/input.txt"
  let
    engine = parseSchematics lines
  log $ show $ "Part1, sum of part nums: "
    <> (show $ sum $ map _.num $ filter (\part -> isAdjacentToAnySymbol part engine.symbols) engine.parts)
  log $ show $ "Part2, sum of gear ratios: "
    <> (show $ sum $ mapMaybe (getGearRatio engine.parts) engine.symbols)

combineSchematics :: EngineSchematics -> EngineSchematics -> EngineSchematics
combineSchematics a b = { parts: a.parts <> b.parts, symbols: a.symbols <> b.symbols }

emptySchematics :: EngineSchematics
emptySchematics = { parts: Nil, symbols: Nil }

isAdjacentToAnySymbol :: EnginePart -> List EngineSymbol -> Boolean
isAdjacentToAnySymbol part symbols = any (isNextToSymbol part) symbols

findAdjacentParts :: EngineSymbol -> List EnginePart -> List EnginePart
findAdjacentParts symbol parts = filter (\part -> isNextToSymbol part symbol) parts

getGearRatio :: List EnginePart -> EngineSymbol -> Maybe Int
getGearRatio parts symbol
  | symbol.symbol == "*" = case findAdjacentParts symbol parts of
    (a : b : Nil) -> Just (a.num * b.num)
    _ -> Nothing
getGearRatio _ _ = Nothing

isAdjacentToExactlyTwoParts :: EngineSymbol -> List EnginePart -> Boolean
isAdjacentToExactlyTwoParts symbol parts = 2 == List.length (filter (\part -> isNextToSymbol part symbol) parts)

isNextToSymbol :: EnginePart -> EngineSymbol -> Boolean
isNextToSymbol part symbol =
  (symbol.col >= part.col - 1 && symbol.col <= part.col + part.length)
    && (symbol.row >= part.row - 1 && symbol.row <= part.row + 1)

parseSchematics :: Array String -> EngineSchematics
parseSchematics inputRows = go 0 inputRows
  where
  go row inputRows' = case uncons inputRows' of
    Just { head: x, tail: xs } -> combineSchematics (parseSchematicsRow row x) (go (row + 1) xs)
    Nothing -> emptySchematics

parseSchematicsRow :: Int -> String -> EngineSchematics
parseSchematicsRow _ "" = emptySchematics

parseSchematicsRow row input = go 0 row input
  where
  go _ _ "" = emptySchematics

  go col' row' input' =
    let
      partNum = takeWhile isDecDigit input'

      symbol = takeWhile isSymbol input'

      remaining = dropWhile isPeriod (drop (length partNum + length symbol) input')

      droppedNum = (length input') - (length remaining)
    in
      case { p: partNum, s: symbol } of
        { p }
          | p /= "" -> addEnginePart (go (col' + droppedNum) row remaining) { col: col', row: row', num: parseNum (partNum), length: length partNum }
        { s }
          | s /= "" -> addEngineSymbol (go (col' + droppedNum) row remaining) { col: col', row: row', symbol: s }
        _ -> go (col' + droppedNum) row' remaining

parseNum :: String -> Int
parseNum s = fromMaybe 0 (fromString s)

isPeriod :: CodePoint -> Boolean
isPeriod cp = cp == codePointFromChar '.'

isSymbol :: CodePoint → Boolean
isSymbol = (not isPeriod) && (not isDecDigit)

addEnginePart :: EngineSchematics -> EnginePart -> EngineSchematics
addEnginePart e ep = e { parts = ep : e.parts }

addEngineSymbol :: EngineSchematics -> EngineSymbol -> EngineSchematics
addEngineSymbol e symbol = e { symbols = symbol : e.symbols }
