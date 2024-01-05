module Main where

import Prelude

import Data.Array (head, last)
import Data.Either (hush)
import Data.Foldable (foldMap, product)
import Data.Int (toNumber)
import Data.List (List(..), zip)
import Data.Maybe (fromMaybe)
import Data.Number (fromString)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import Parsing (Parser, runParser)
import Parsing.Combinators (manyTill, sepBy)
import Parsing.String (anyCodePoint, string)
import Parsing.String.Basic (intDecimal, skipSpaces)
import Utils (readLines)

type Race
  = { time :: Number, record :: Number }

main :: Effect Unit
main = do
  inputLines <- readLines "06-wait-for-it/input.txt"
  let
    lines = map parseNumbers inputLines

    times = fromMaybe Nil (head lines)

    records = fromMaybe Nil (last lines)

    races = zip times records # map (\(time /\ record) -> { time: toNumber time, record: toNumber record })

    longRace = {time: joinDigits times, record: joinDigits records}

  log $ show $ "Part1: "
    <> (show $ product $ map (\race -> numberOfWaysToBeatTheRecord race) races)

  log $ show $ "Part2: "
    <> (show $ numberOfWaysToBeatTheRecord longRace)

joinDigits :: List Int -> Number
joinDigits digits = fromMaybe 0.0 (fromString $ foldMap show digits)

numberOfWaysToBeatTheRecord :: Race -> Number
numberOfWaysToBeatTheRecord race = go 0.0 race 0.0 where 
  go way race' acc | way > race'.time = acc
  go way race' acc = case distance race'.time way > race'.record of
        true -> go (way + 1.0) race' (acc + 1.0)
        _ -> go (way + 1.0) race' acc

distance :: Number -> Number -> Number
distance raceTime buttonTime
  | buttonTime >= raceTime = 0.0
distance raceTime buttonTime = (raceTime - buttonTime) * buttonTime

parseNumbers :: String -> List Int
parseNumbers input = fromMaybe Nil (hush $ runParser input numbersParser)

numbersParser :: Parser String (List Int)
numbersParser = manyTill anyCodePoint (string ":") *> skipSpaces *> intDecimal `sepBy` skipSpaces
