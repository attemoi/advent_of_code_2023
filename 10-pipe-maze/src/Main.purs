module Main where

import Prelude
import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (hush)
import Data.Foldable (sum)
import Data.List (List(..), catMaybes, filter, fromFoldable, head, (:))
import Data.List.NonEmpty (toList)
import Data.Map (Map, lookup)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (toUnfoldable)
import Data.Set as Set
import Data.Tuple (Tuple, fst, snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import Parsing (Parser)
import Parsing as Parsing
import Parsing.String (string)
import Parsing.String.Replace (splitCap)
import Utils (column, line, readFile)

data Direction
  = North
  | East
  | South
  | West

derive instance eqDirection :: Eq Direction

type Position
  = { x :: Int, y :: Int }

opposite :: Direction -> Direction
opposite North = South

opposite South = North

opposite West = East

opposite East = West

move :: Position -> Direction -> Position
move pos North = pos { y = pos.y - 1 }

move pos South = pos { y = pos.y + 1 }

move pos East = pos { x = pos.x + 1 }

move pos West = pos { x = pos.x - 1 }

type Pipe
  = Tuple Direction Direction

type PipeMap
  = Map Position Pipe

main :: Effect Unit
main = do
  input <- readFile "10-pipe-maze/input.txt"
  let
    pipeMap = parsePipes input

    startPosition = findStartPosition input

    startPipe = findStartPipe startPosition pipeMap

    pipesInLoop = traversePipeline startPosition startPipe pipeMap
  log $ show $ "Part 1, steps to furthest point in pipeline: "
    <> (show $ Map.size pipesInLoop / 2)
  log $ show $ "Part 2, number of enclosed tiles: "
    <> (show $ enclosedTiles pipesInLoop)

findStartPosition :: String -> Position
findStartPosition input =
  fromMaybe { x: 0, y: 0 } $ head $ catMaybes $ toList $ hush
    <$> splitCap input startPositionParser

findStartPipe :: Position -> PipeMap -> Pipe
findStartPipe startPosition pipeMap = case filter (connectsToDir startPosition) (North : East : South : West : Nil) of
  a : b : _ -> a /\ b
  _ -> North /\ South -- This shouldn't happen
  where
  connectsToDir position dir = case lookup position pipeMap of
    Nothing -> false
    Just pipe -> (fst pipe == opposite dir) || (snd pipe == opposite dir)

parsePipes :: String -> PipeMap
parsePipes input = Map.fromFoldable $ catMaybes $ toList $ hush <$> splitCap input tileParser

enclosedTiles :: PipeMap -> Int
enclosedTiles pipes = pointsInPolygon pipes

pointsInPolygon :: PipeMap -> Int
pointsInPolygon pipeMap = sum $ map pointsInRow rows
  where
  northConnectingPipePositions = Map.keys $ Map.filter connectsNorth pipeMap

  rows = map fromFoldable $ Array.groupAllBy (comparing _.y) $ toUnfoldable northConnectingPipePositions

  pointsInRow (a : b : xs) = b.x - a.x - 1 - (numPipesBetween a b) + (pointsInRow xs)

  pointsInRow _ = 0

  numPipesBetween a b =
    Map.filterWithKey (\k _ -> k.y == a.y) pipeMap
      # Map.keys
      # Set.filter (\pos -> a.x < pos.x && pos.x < b.x)
      # Set.size

connectsNorth :: Pipe -> Boolean
connectsNorth (a /\ b) = a == North || b == North

traversePipeline :: Position -> Pipe -> PipeMap -> PipeMap
traversePipeline startPosition startPipe allPipes = go Map.empty startPosition startPipe (fst startPipe)
  where
  go acc position pipe fromDir =
    let
      outDir = otherEndDir pipe fromDir

      nextPosition = (move position outDir)

      nextPipe = lookup nextPosition allPipes
    in
      case nextPipe of
        Nothing -> Map.insert position pipe acc
        (Just p) -> go (Map.insert position pipe acc) nextPosition p (opposite outDir)

otherEndDir :: Pipe -> Direction -> Direction
otherEndDir pipe direction
  | fst pipe == direction = snd pipe
  | snd pipe == direction = fst pipe
  | otherwise = snd pipe

startPositionParser :: Parser String Position
startPositionParser = do
  pos <- Parsing.position
  _ <- string "S"
  pure ({ x: column pos, y: line pos })

tileParser :: Parser String (Tuple Position Pipe)
tileParser = do
  position <- Parsing.position
  directions <-
    (string "|" $> North /\ South)
      <|> (string "-" $> East /\ West)
      <|> (string "L" $> North /\ East)
      <|> (string "J" $> North /\ West)
      <|> (string "7" $> South /\ West)
      <|> (string "F" $> South /\ East)
  pure ({ x: column position, y: line position } /\ directions)
