module Main where

import Prelude
import Data.Either (Either(..))
import Data.Foldable (minimum)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import Parsing (Parser, runParser)
import Parsing.Combinators (many, manyTill, sepBy, try)
import Parsing.String (anyTill, char, string)
import Parsing.String.Basic (number, skipSpaces)
import Range (Range(..), start, spliceAfter, spliceBefore, spliceOverlapping)
import Utils (readFile)

type CategoryMap
  = List Conversion

type Conversion
  = { range :: Range, diff :: Number }

main :: Effect Unit
main = do
  input <- readFile "05-seeds/input.txt"
  let
    (seeds /\ categoryMaps) = case runParser (input) inputParser of
      Left _ -> (Nil /\ Nil)
      Right r -> r

    -- For part 1, consider each number a range of one number
    rangesOfOne = map (\seed -> Range seed seed) seeds

    -- For part 2, convert number pairs to ranges
    seedRanges = toRanges seeds
  log $ show $ "Part1: " <> (show $ minimum $ map start $ convert rangesOfOne categoryMaps)
  log $ show $ "Part2: " <> (show $ minimum $ map start $ convert seedRanges categoryMaps)

toRanges :: List Number -> List Range
toRanges (s : length : tail) = Range s (s + length - 1.0) : toRanges (tail)

toRanges _ = Nil

convert :: List Range -> List CategoryMap -> List Range
convert Nil _ = Nil

convert ranges Nil = ranges

convert (r : rs) cms = convert' r cms <> convert rs cms

convert' :: Range -> List CategoryMap -> List Range
convert' range Nil = range : Nil

convert' range (cm : cms) = convert (convert'' range cm) cms

convert'' :: Range -> CategoryMap -> List Range
convert'' range Nil = range : Nil

convert'' range (c : cs) =
  let
    before = case spliceBefore range c.range of
      Nothing -> Nil
      Just r -> convert'' r cs

    after = case spliceAfter range c.range of
      Nothing -> Nil
      Just r -> convert'' r cs

    overlapConverted = case spliceOverlapping range c.range of
      Nothing -> Nil
      Just (Range a b) -> Range (a + c.diff) (b + c.diff) : Nil
  in
    before <> overlapConverted <> after

inputParser :: Parser String (Tuple (List Number) (List CategoryMap))
inputParser = do
  _ <- string "seeds: "
  seeds <- try $ number `sepBy` (char ' ')
  skipSpaces
  categoryMap <- many categoryMapParser
  pure (seeds /\ categoryMap)

categoryMapParser :: Parser String CategoryMap
categoryMapParser = do
  _ <- anyTill (string "map:")
  skipSpaces
  ranges <- manyTill conversionParser (char '\n' $> char '\n')
  pure (ranges)

conversionParser :: Parser String Conversion
conversionParser = do
  destinationStart <- number
  skipSpaces
  sourceStart <- number
  skipSpaces
  length <- number
  _ <- char '\n'
  pure { range: Range sourceStart (sourceStart + length - 1.0), diff: destinationStart - sourceStart }
