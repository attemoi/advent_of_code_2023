module Main where

import Prelude

import Data.Array (mapMaybe)
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Int (fromString)
import Data.List (List(..), head, last, (:))
import Data.Maybe (Maybe)
import Data.String (Pattern(..), drop, split)
import Data.String.CodeUnits (fromCharArray)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Parsing (Parser, runParser)
import Parsing.Combinators ((<|>))
import Parsing.String (string)
import Parsing.String.Basic (digit)

main :: Effect Unit
main = do
  let filePath = "01-trebuchet/input.txt"
  input <- readTextFile UTF8 filePath
  let lines = split (Pattern "\n") input
  log $ show $ "part1: " <> 
    (show $ sum $ mapMaybe (calibrationValue digit) lines)
  log $ show $ "part2: " <> 
    (show $ sum $ mapMaybe (calibrationValue (digit <|> textualDigit)) lines)

calibrationValue :: Parser String Char -> String -> Maybe Int
calibrationValue parser input = do
  let digits = parseAll parser input
  first <- head digits
  last <- last digits
  combined <- fromString $ fromCharArray $ [first, last]
  pure (combined)

-- | Parse all instances matching with the given parser
parseAll :: forall a. Parser String a -> String -> List a
parseAll _ "" = Nil
parseAll parser input = case runParser input parser of
    Left _ -> parseAll parser (drop 1 input)
    Right c -> c : parseAll parser (drop 1 input)

textualDigit :: Parser String Char
textualDigit = (string "zero" >>= \_ -> pure '0')
  <|> (string "one" >>= \_ -> pure '1')
  <|> (string "two" >>= \_ -> pure '2')
  <|> (string "three" >>= \_ -> pure '3')
  <|> (string "four" >>= \_ -> pure '4')
  <|> (string "five" >>= \_ -> pure '5')
  <|> (string "six" >>= \_ -> pure '6')
  <|> (string "seven" >>= \_ -> pure '7')
  <|> (string "eight" >>= \_ -> pure '8')
  <|> (string "nine" >>= \_ -> pure '9')
