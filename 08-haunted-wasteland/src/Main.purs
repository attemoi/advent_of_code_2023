module Main where

import Prelude
import Data.Array (mapMaybe)
import Data.Array as Array
import Data.Either (hush)
import JS.BigInt as BigInt
import Data.List (List(..), foldl, snoc, (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String.Utils (endsWith)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import Parsing (Parser, runParser)
import Parsing.Combinators (many, (<|>))
import Parsing.String (string, takeN)
import Utils (readLines)

data Instruction
  = Left
  | Right

type Vertex
  = String

type Edges
  = { left :: String, right :: String }

type Graph
  = Map Vertex Edges

setToList :: forall a. Set a -> List a
setToList = Set.toUnfoldable

main :: Effect Unit
main = do
  lines <- readLines "08-haunted-wasteland/input.txt"
  let
    graph = Map.fromFoldable $ mapMaybe parseVertex (Array.drop 2 lines)

    instructions = parseInstructions $ fromMaybe "" (Array.head lines)

    ghostStartVertices = setToList $ Map.keys $ Map.filterKeys (endsWith "A") graph

    movesForEachGhost = map (countMovesUntil (endsWith "Z") graph instructions) ghostStartVertices

    leastCommonMultipleForEachGhost = foldl lcm (BigInt.fromInt 1) (map BigInt.fromInt movesForEachGhost)
  log $ show $ "Part 1 (steps until ZZZ): "
    <> (show $ countMovesUntil (_ == "ZZZ") graph instructions "AAA")
  log $ show $ "Part 2 (steps for all ghosts to end up on nodes ending with Z): "
    <> (show $ leastCommonMultipleForEachGhost)

nextVertexAll :: Graph -> List Vertex -> Instruction -> List Vertex
nextVertexAll graph vertices instruction = map (nextVertex graph instruction) vertices

countMovesUntil :: (Vertex -> Boolean) -> Graph -> List Instruction -> Vertex -> Int
countMovesUntil predicate graph instructions startVertex = go 0 instructions startVertex
  where
  go acc Nil _ = acc

  go acc _ vertex
    | predicate vertex = acc

  go acc (i : is) vertex = go (acc + 1) (snoc is i) (nextVertex graph i vertex)

nextVertex :: Graph -> Instruction -> Vertex -> Vertex
nextVertex graph i vertex = case Map.lookup vertex graph of
  Just e -> case i of
    Left -> e.left
    Right -> e.right
  Nothing -> vertex

parseVertex :: String -> Maybe (Tuple Vertex Edges)
parseVertex input = hush $ runParser input vertexParser

vertexParser :: Parser String (Tuple Vertex Edges)
vertexParser = do
  name <- takeN 3
  _ <- string " = ("
  left <- takeN 3
  _ <- string ", "
  right <- takeN 3
  pure (name /\ { left: left, right: right })

parseInstructions :: String -> List Instruction
parseInstructions input = fromMaybe Nil $ hush $ runParser input instructionsParser

instructionsParser :: Parser String (List Instruction)
instructionsParser = many (string "L" $> Left <|> string "R" $> Right)
