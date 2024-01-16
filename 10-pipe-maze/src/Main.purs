module Main where

import Prelude
import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (hush)
import Data.Foldable (sum)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), catMaybes, filter, find, groupBy, head, length, sortBy, (:))
import Data.List.NonEmpty (toList)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Show.Generic (genericShow)
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

derive instance genericDirection :: Generic Direction _

instance showDirection :: Show Direction where
  show = genericShow

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
  = { pos :: Position, directions :: Tuple Direction Direction }

main :: Effect Unit
main = do
  input <- readFile "10-pipe-maze/input.txt"
  let
    pipes = parsePipes input

    startPipe = findStartPipe input pipes

    pipesInLoop = traversePipeline startPipe pipes
  log $ show $ "Part 1, steps to furthest point in pipeline: "
    <> (show $ length pipesInLoop / 2)
  log $ show $ "Part 2, number of enclosed tiles: "
    <> (show $ enclosedTiles pipesInLoop)

findStartPipe :: String -> List Pipe -> Pipe
findStartPipe input otherPipes = case determineStartPipe startPos otherPipes of
  Nothing -> { pos: startPos, directions: North /\ West }
  Just a -> a
  where
  startPos = fromMaybe { x: 0, y: 0 } $ head $ catMaybes $ toList $ hush <$> splitCap input startPositionParser

determineStartPipe :: Position -> List Pipe -> Maybe Pipe
determineStartPipe startPos pipes =
  let
    directionsThatConnect =
      filter
        (\dir -> connectsToDir startPos dir pipes)
        (North : East : South : West : Nil)
  in
    case directionsThatConnect of
      a : b : _ -> Just { pos: startPos, directions: a /\ b }
      _ -> Nothing

parsePipes :: String -> List Pipe
parsePipes input = catMaybes $ toList $ hush <$> splitCap input tileParser

enclosedTiles :: List Pipe -> Int
enclosedTiles pipes = pointsInPolygon pipes

pointsInPolygon :: List Pipe -> Int
pointsInPolygon pipes = sum $ map pointsInRow rows
  where
  rows =
    sortPipes pipes
      # groupBy (\a b -> a.pos.y == b.pos.y)
      # map toList
      # map (sortBy (\a b -> compare a.pos.x b.pos.x))

-- Got stack overflow when sorting list, so converting to array and back here
sortPipes :: List Pipe -> List Pipe
sortPipes pipes =
  Array.fromFoldable pipes
    # Array.sortWith (_.pos.x)
    # Array.sortWith (_.pos.y)
    # Array.toUnfoldable

pointsInRow :: List Pipe -> Int
pointsInRow pipes = go northConnectingPipes
  where
  northConnectingPipes = filter connectsNorth pipes

  go (a : b : xs) = b.pos.x - a.pos.x - 1 - (numPipesBetween a b pipes) + (go xs)

  go _ = 0

numPipesBetween :: Pipe -> Pipe -> List Pipe -> Int
numPipesBetween a b pipeRow =
  length
    $ filter
        (\pipe -> a.pos.x < pipe.pos.x && pipe.pos.x < b.pos.x)
        pipeRow

connectsNorth :: Pipe -> Boolean
connectsNorth { directions: a /\ b } = a == North || b == North

row :: Int -> List Pipe -> List Pipe
row num pipes = filter (\pipe -> pipe.pos.y == num) pipes

traversePipeline :: Pipe -> List Pipe -> List Pipe
traversePipeline startPipe allPipes = go Nil startPipe (fst startPipe.directions)
  where
  go acc pipe fromDir =
    let
      outDir = otherEndDir pipe fromDir

      nextPipe = pipeInPos (move pipe.pos outDir) allPipes
    in
      case nextPipe of
        Nothing -> pipe : acc
        (Just p) -> go (pipe : acc) p (opposite outDir)

connectsToDir :: Position -> Direction -> List Pipe -> Boolean
connectsToDir pos dir pipes = case pipeInPos (move pos dir) pipes of
  Nothing -> false
  Just p -> (fst p.directions == opposite dir) || (snd p.directions == opposite dir)

otherEndDir :: Pipe -> Direction -> Direction
otherEndDir pipe direction
  | fst pipe.directions == direction = snd pipe.directions
  | snd pipe.directions == direction = fst pipe.directions
  | otherwise = snd pipe.directions

pipeInPos :: Position -> List Pipe -> Maybe Pipe
pipeInPos pos pipes = find (\pipe -> pipe.pos == pos) pipes

startPositionParser :: Parser String Position
startPositionParser = do
  pos <- Parsing.position
  _ <- string "S"
  pure ({ x: column pos, y: line pos })

tileParser :: Parser String Pipe
tileParser = do
  pos <- Parsing.position
  directions <-
    (string "|" $> North /\ South)
      <|> (string "-" $> East /\ West)
      <|> (string "L" $> North /\ East)
      <|> (string "J" $> North /\ West)
      <|> (string "7" $> South /\ West)
      <|> (string "F" $> South /\ East)
  pure ({ pos: { x: column pos, y: line pos }, directions: directions })
