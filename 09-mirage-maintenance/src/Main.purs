module Main where

import Prelude
import Data.Array as Array
import Data.Either (hush)
import Data.Foldable (all, sum)
import Data.List (List(..), head, last, zipWith, (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple, fst, snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import Parsing (Parser, runParser)
import Parsing.Combinators (sepBy)
import Parsing.String.Basic (intDecimal, skipSpaces)
import Utils (readLines)

main :: Effect Unit
main = do
  inputLines <- readLines "09-mirage-maintenance/input.txt"
  let
    valueHistories = Array.mapMaybe parseValueHistory inputLines

    extrapolations = Array.mapMaybe extrapolate valueHistories
  log $ show $ "Part 1, sum of forwards extrapolated values: "
    <> (show $ sum $ map snd $ extrapolations)
  log $ show $ "Part 2, sum of backwards extrapolated values: "
    <> (show $ sum $ map fst $ extrapolations)

extrapolate :: List Int -> Maybe (Tuple Int Int)
extrapolate Nil = Nothing

extrapolate list
  | all (_ == 0) list = Just (0 /\ 0)
  | otherwise = do
    last <- last list
    head <- head list
    (b /\ f) <- extrapolate (differences list)
    pure ((head - b) /\ (last + f))

differences :: List Int -> List Int
differences Nil = Nil

differences (x : xs) = zipWith (-) xs (x : xs)

parseValueHistory :: String -> Maybe (List Int)
parseValueHistory input = hush $ runParser input valueHistory

valueHistory :: Parser String (List Int)
valueHistory = intDecimal `sepBy` skipSpaces
