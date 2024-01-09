module Main where

import Prelude
import Data.Either (Either(..))
import Data.Foldable (minimum)
import Data.List (List(..), mapMaybe, (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple, fst)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import Parsing (Parser, runParser)
import Parsing.Combinators (many, manyTill, sepBy, try)
import Parsing.String (anyTill, char, string)
import Parsing.String.Basic (number, skipSpaces)
import Utils (readFile)

type Seeds
  = List Number

type Range
  = { destinationStart :: Number, sourceStart :: Number, length :: Number }

type CategoryMap
  = List Range

main :: Effect Unit
main = do
  input <- readFile "05-seeds/input.txt"
  let
    -- TODO: Fix hack with input requiring two newlines. 
    -- Can't figure out how to parse input with multiple inner `sepBy` clauses
    (seeds /\ categoryMaps) = case runParser (input <> "\n\n") inputParser of
      Left _ -> (Nil /\ Nil)
      Right r -> r

    converted = map (seedLocation categoryMaps) seeds
  log $ show $ minimum $ converted

seedLocation :: List CategoryMap -> Number -> Number
seedLocation Nil seed = seed

seedLocation (cm : cms) seed = seedLocation cms (convertCategory seed cm)

convertCategory :: Number -> CategoryMap -> Number
convertCategory number categoryMap = case mapMaybe (convertRange number) categoryMap of
  Nil -> number
  (converted : _) -> converted

convertRange :: Number -> Range -> Maybe Number
convertRange number range
  | number < range.sourceStart = Nothing
  | number > range.sourceStart + range.length - 1.0 = Nothing
  | otherwise = Just $ range.destinationStart + (number - range.sourceStart)

inputParser :: Parser String (Tuple Seeds (List CategoryMap))
inputParser = do
  _ <- string "seeds: "
  seeds <- try $ number `sepBy` (char ' ')
  skipSpaces
  categoryMap <- many categoryMapParser
  pure (seeds /\ categoryMap)

categoryMapParser :: Parser String CategoryMap
categoryMapParser = do
  _ <- fst <$> anyTill (string "map:")
  skipSpaces
  ranges <- manyTill rangeParser (char '\n' $> char '\n')
  pure (ranges)

rangeParser :: Parser String Range
rangeParser = do
  destinationStart <- number
  skipSpaces
  sourceStart <- number
  skipSpaces
  length <- number
  _ <- char '\n'
  pure ({ destinationStart, sourceStart, length })
