module Main where

import Prelude
import Data.Array (mapMaybe)
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Int (fromString)
import Data.List (List(..), head, last, (:))
import Data.Maybe (Maybe)
import Data.String (drop)
import Data.String.CodeUnits (fromCharArray)
import Effect (Effect)
import Effect.Console (log)
import Parsing (Parser, runParser)
import Parsing.Combinators ((<|>))
import Parsing.String (string)
import Parsing.String.Basic (digit)
import Utils (readLines)

main :: Effect Unit
main = do
  lines <- readLines "01-trebuchet/input.txt"
  log $ show $ "part1: "
    <> (show $ sum $ mapMaybe (calibrationValue digit) lines)
  log $ show $ "part2: "
    <> (show $ sum $ mapMaybe (calibrationValue (digit <|> textualDigit)) lines)

calibrationValue :: Parser String Char -> String -> Maybe Int
calibrationValue parser input = do
  let
    digits = parseAll parser input
  first <- head digits
  last <- last digits
  combined <- fromString $ fromCharArray $ [ first, last ]
  pure (combined)

-- | Parse all instances matching with the given parser
parseAll :: forall a. Parser String a -> String -> List a
parseAll _ "" = Nil

parseAll parser input = case runParser input parser of
  Left _ -> parseAll parser (drop 1 input)
  Right c -> c : parseAll parser (drop 1 input)

textualDigit :: Parser String Char
textualDigit =
  (string "zero" $> '0')
    <|> (string "one" $> '1')
    <|> (string "two" $> '2')
    <|> (string "three" $> '3')
    <|> (string "four" $> '4')
    <|> (string "five" $> '5')
    <|> (string "six" $> '6')
    <|> (string "seven" $> '7')
    <|> (string "eight" $> '8')
    <|> (string "nine" $> '9')
