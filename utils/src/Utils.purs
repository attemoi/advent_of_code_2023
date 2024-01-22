module Utils
  ( readFile
  , readLines
  , column
  , line
  , parseGrid
  , Coords
  ) where

import Prelude
import Data.Either (hush)
import Data.List (List, catMaybes)
import Data.List.NonEmpty (toList)
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Parsing (Parser, Position(..), position)
import Parsing.String.Replace (splitCap)

readFile :: String -> Effect String
readFile filePath = readTextFile UTF8 filePath

readLines :: String -> Effect (Array String)
readLines filePath = do
  fileContent <- readTextFile UTF8 filePath
  pure (split (Pattern "\n") fileContent)

column :: Position -> Int
column (Position a) = _.column a

line :: Position -> Int
line (Position a) = _.line a

type Coords
  = { x :: Int, y :: Int }

parseGrid :: forall a. String -> Parser String a -> List (Tuple Coords a)
parseGrid input parser = catMaybes $ toList $ hush <$> splitCap input (gridParser parser)

gridParser :: forall a. Parser String a -> Parser String (Tuple Coords a)
gridParser parser = do
  position <- position
  parsed <- parser
  pure ({ x: column position, y: line position } /\ parsed)
