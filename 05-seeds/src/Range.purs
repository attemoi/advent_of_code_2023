module Range where

import Prelude
import Data.Maybe (Maybe(..))

data Range
  = Range Number Number

start :: Range -> Number
start (Range a _) = a

end :: Range -> Number
end (Range _ b) = b

rangesOverlap :: Range -> Range -> Boolean
rangesOverlap a b = isWithinRange (start a) b || isWithinRange (end a) b

isWithinRange :: Number -> Range -> Boolean
isWithinRange number range = number >= start range && number <= end range

spliceBefore :: Range -> Range -> Maybe Range
spliceBefore a b
  | start a >= start b = Nothing
  | otherwise = Just $ Range (start a) (min (end a) (start b - 1.0))

spliceAfter :: Range -> Range -> Maybe Range
spliceAfter a b
  | end a <= end b = Nothing
  | otherwise = Just $ Range (max (start a) (end b + 1.0)) (end a)

spliceOverlapping :: Range -> Range -> Maybe Range
spliceOverlapping a b
  | not rangesOverlap a b = Nothing
  | otherwise =
    let
      overlapStart = max (start a) (start b)

      overlapEnd = min (end a) (end b)
    in
      Just
        $ Range overlapStart overlapEnd
