module Main where

import Prelude
import Control.Alternative (guard)
import Data.Foldable (maximum, minimum, sum)
import Data.List (range)
import Data.List as List
import Data.Maybe (fromMaybe)
import Data.Ord (abs)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple, fst)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import Parsing.String (string)
import Utils (readFile, Coords, parseGrid)

type Galaxies
  = Set Coords

type Bounds
  = { minX :: Int
    , minY :: Int
    , maxX :: Int
    , maxY :: Int
    }

main :: Effect Unit
main = do
  input <- readFile "11-cosmic-expansion/input.txt"
  let
    galaxies = parseGalaxies input

    expandedGalaxies = expandUniverse galaxies

    galaxyPairs = uniquePairs expandedGalaxies
  log $ show $ "Part1 (sum of shortest paths): "
    <> (show $ sum $ map pathLength $ List.fromFoldable $ galaxyPairs)

pathLength :: Tuple Coords Coords -> Int
pathLength (a /\ b) = (abs (b.y - a.y)) + (abs (b.x - a.x))

uniquePairs :: Galaxies -> Set (Tuple Coords Coords)
uniquePairs set =
  Set.fromFoldable
    $ do
        x <- List.fromFoldable set
        y <- List.fromFoldable set
        guard (x < y)
        pure $ x /\ y

getBounds :: Galaxies -> Bounds
getBounds galaxies =
  { minX: fromMaybe 0 $ minimum $ Set.map _.x $ galaxies
  , minY: fromMaybe 0 $ minimum $ Set.map _.y $ galaxies
  , maxX: fromMaybe 0 $ maximum $ Set.map _.x $ galaxies
  , maxY: fromMaybe 0 $ maximum $ Set.map _.y $ galaxies
  }

expandUniverse :: Galaxies -> Galaxies
expandUniverse galaxies = Set.map (expandGalaxy emptyRows emptyColumns) galaxies
  where
  bounds = getBounds galaxies

  emptyRows = Set.difference allRows rowsWithGalaxies
    where
    allRows = Set.fromFoldable $ range bounds.minY bounds.maxY

    rowsWithGalaxies = Set.map _.y $ galaxies

  emptyColumns = Set.difference allColumns columnsWithGalaxies
    where
    allColumns = Set.fromFoldable $ range bounds.minX bounds.maxX

    columnsWithGalaxies = Set.map _.x $ galaxies

expandGalaxy :: Set Int -> Set Int -> Coords -> Coords
expandGalaxy emptyRows emptyColumns galaxyCoords =
  { x: galaxyCoords.x + columnExpansion
  , y: galaxyCoords.y + rowExpansion
  }
  where
  rowExpansion = Set.size $ Set.filter (_ < galaxyCoords.y) emptyRows

  columnExpansion = Set.size $ Set.filter (_ < galaxyCoords.x) emptyColumns

parseGalaxies :: String -> Galaxies
parseGalaxies input = Set.fromFoldable $ map fst $ parseGrid input (string "#")
