module Main where

import Prelude
import Control.Alt ((<|>))
import Data.Array as Array
import Data.Foldable (sum)
import Data.List (List(..), filter, fromFoldable, head, (:))
import Data.Map (Map, lookup)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (toUnfoldable)
import Data.Set as Set
import Data.Tuple (Tuple, fst, snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import Parsing (Parser)
import Parsing.String (string)
import Utils (Coords, parseGrid, readFile)

data Direction
  = North
  | East
  | South
  | West

derive instance eqDirection :: Eq Direction

opposite :: Direction -> Direction
opposite North = South

opposite South = North

opposite West = East

opposite East = West

move :: Coords -> Direction -> Coords
move pos North = pos { y = pos.y - 1 }

move pos South = pos { y = pos.y + 1 }

move pos East = pos { x = pos.x + 1 }

move pos West = pos { x = pos.x - 1 }

type Pipe
  = Tuple Direction Direction

type PipeMap
  = Map Coords Pipe

main :: Effect Unit
main = do
  input <- readFile "10-pipe-maze/input.txt"
  let
    pipeMap = parsePipes input

    startCoords = findStartCoords input

    startPipe = findStartPipe startCoords pipeMap

    pipesInLoop = traversePipeline startCoords startPipe pipeMap
  log $ show $ "Part 1, steps to furthest point in pipeline: "
    <> (show $ Map.size pipesInLoop / 2)
  log $ show $ "Part 2, number of enclosed tiles: "
    <> (show $ enclosedTiles pipesInLoop)

findStartCoords :: String -> Coords
findStartCoords input = case head $ parseGrid input (string "S") of
  Nothing -> { x: 0, y: 0 }
  Just (coords /\ _) -> coords

findStartPipe :: Coords -> PipeMap -> Pipe
findStartPipe startCoords pipeMap = case filter (connectsToDir startCoords) (North : East : South : West : Nil) of
  a : b : _ -> a /\ b
  _ -> North /\ South -- This shouldn't happen
  where
  connectsToDir position dir = case lookup position pipeMap of
    Nothing -> false
    Just pipe -> (fst pipe == opposite dir) || (snd pipe == opposite dir)

parsePipes :: String -> PipeMap
parsePipes input = Map.fromFoldable $ parseGrid input tileParser

enclosedTiles :: PipeMap -> Int
enclosedTiles pipes = pointsInPolygon pipes

pointsInPolygon :: PipeMap -> Int
pointsInPolygon pipeMap = sum $ map pointsInRow rows
  where
  northConnectingPipeCoords = Map.keys $ Map.filter connectsNorth pipeMap

  rows = map fromFoldable $ Array.groupAllBy (comparing _.y) $ toUnfoldable northConnectingPipeCoords

  pointsInRow (a : b : xs) = b.x - a.x - 1 - (numPipesBetween a b) + (pointsInRow xs)

  pointsInRow _ = 0

  numPipesBetween a b =
    Map.filterWithKey (\k _ -> k.y == a.y) pipeMap
      # Map.keys
      # Set.filter (\pos -> a.x < pos.x && pos.x < b.x)
      # Set.size

connectsNorth :: Pipe -> Boolean
connectsNorth (a /\ b) = a == North || b == North

traversePipeline :: Coords -> Pipe -> PipeMap -> PipeMap
traversePipeline startCoord startPipe allPipes = go Map.empty startCoord startPipe (fst startPipe)
  where
  go acc position pipe fromDir =
    let
      outDir = otherEndDir pipe fromDir

      nextCoord = (move position outDir)

      nextPipe = lookup nextCoord allPipes
    in
      case nextPipe of
        Nothing -> Map.insert position pipe acc
        (Just p) -> go (Map.insert position pipe acc) nextCoord p (opposite outDir)

otherEndDir :: Pipe -> Direction -> Direction
otherEndDir pipe direction
  | fst pipe == direction = snd pipe
  | snd pipe == direction = fst pipe
  | otherwise = snd pipe

tileParser :: Parser String Pipe
tileParser =
  (string "|" $> North /\ South)
    <|> (string "-" $> East /\ West)
    <|> (string "L" $> North /\ East)
    <|> (string "J" $> North /\ West)
    <|> (string "7" $> South /\ West)
    <|> (string "F" $> South /\ East)
