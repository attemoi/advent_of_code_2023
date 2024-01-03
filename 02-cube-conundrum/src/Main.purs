module Main where

import Prelude
import Data.Array (filter, mapMaybe)
import Data.Either (hush)
import Data.Foldable (all, foldl, maximum, sum)
import Data.List (List)
import Data.Maybe (Maybe, fromMaybe)
import Effect (Effect)
import Effect.Console (log)
import Parsing (Parser, runParser)
import Parsing.Combinators (sepBy, (<|>))
import Parsing.String (string)
import Parsing.String.Basic (intDecimal, skipSpaces)
import Utils (readLines)

type Game
  = { id :: Int, rounds :: List Round }

type Round
  = { red :: Int, green :: Int, blue :: Int }

data Cubes
  = Red Int
  | Green Int
  | Blue Int

main :: Effect Unit
main = do
  lines <- readLines "02-cube-conundrum/input.txt"
  let
    games = mapMaybe parseGame lines
  log $ show $ "Part1: "
    <> (show $ sum $ map _.id $ filter isPossible games)
  log $ show $ "Part2: "
    <> (show $ sum $ map power $ map minimalSetOfCubes games)

isPossible :: Game -> Boolean
isPossible game = all enoughCubes game.rounds

enoughCubes :: Round -> Boolean
enoughCubes round = round.red <= 12 && round.green <= 13 && round.blue <= 14

minimalSetOfCubes :: Game -> Round
minimalSetOfCubes { rounds } =
  { red: fieldMax rounds _.red
  , green: fieldMax rounds _.green
  , blue: fieldMax rounds _.blue
  }

power :: Round -> Int
power { red, green, blue } = red * green * blue

fieldMax :: List Round -> (Round -> Int) -> Int
fieldMax rounds f = fromMaybe 0 (maximum $ map f rounds)

parseGame :: String -> Maybe Game
parseGame input = hush $ runParser input gameParser

gameParser :: Parser String Game
gameParser = do
  _ <- string "Game "
  gameId <- intDecimal
  _ <- string ": "
  rounds <- roundParser `sepBy` string "; "
  pure ({ id: gameId, rounds: rounds })

roundParser :: Parser String Round
roundParser = do
  cubes <- cubesParser `sepBy` string ", "
  let
    round = foldl addCubesToRound { red: 0, green: 0, blue: 0 } cubes
  pure (round)

addCubesToRound :: Round -> Cubes -> Round
addCubesToRound round (Red n) = round { red = round.red + n }

addCubesToRound round (Green n) = round { green = round.green + n }

addCubesToRound round (Blue n) = round { blue = round.blue + n }

cubesParser :: Parser String Cubes
cubesParser = do
  numberOfCubes <- intDecimal
  _ <- skipSpaces
  cubes <-
    (string "red" >>= \_ -> pure (Red numberOfCubes))
      <|> (string "green" >>= \_ -> pure (Green numberOfCubes))
      <|> (string "blue" >>= \_ -> pure (Blue numberOfCubes))
  pure (cubes)
