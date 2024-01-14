module Main where

import Prelude
import Control.Alt ((<|>))
import Data.Either (hush)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), catMaybes, filter, find, head, length, (:))
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
    pipes = catMaybes $ toList $ hush <$> splitCap input tileParser

    startPos = fromMaybe { x: 0, y: 0 } $ head $ catMaybes $ toList $ hush <$> splitCap input startPositionParser

    startPipe = case determineStartPipe startPos pipes of
      Nothing -> { pos: startPos, directions: North /\ West }
      Just a -> a
  log $ show $ "Part 1, steps to furthest point in pipeline"
    <> (show $ length (traversePipeline startPipe pipes) / 2)

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
