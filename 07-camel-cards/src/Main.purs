module Main where

import Prelude
import Control.Alt ((<|>))
import Data.Array (mapMaybe)
import Data.Either (hush)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Parsing (Parser, runParser)
import Parsing.Combinators.Array (many)
import Parsing.String (string)
import Parsing.String.Basic (intDecimal, skipSpaces)
import Utils (readLines)

type Hand
  = { cards :: Array Card, bid :: Int }

data Card
  = Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace

derive instance eqCard :: Eq Card

derive instance ordCard :: Ord Card

derive instance genericCard :: Generic Card _

instance showCard :: Show Card where
  show = genericShow

data HandType
  = HighCard
  | OnePair
  | TwoPairs
  | ThreeOfKind
  | FullHouse
  | FourOfKind
  | FiveOfKind

derive instance eqHandType :: Eq HandType

derive instance ordHandType :: Ord HandType

main :: Effect Unit
main = do
  inputLines <- readLines "07-camel-cards/input.txt"
  let
    hands = mapMaybe parseHand inputLines
  log $ show $ hands

parseHand :: String -> Maybe Hand
parseHand input = hush $ runParser input handParser

handParser :: Parser String Hand
handParser = do
  cards <- many cardParser
  skipSpaces
  bid <- intDecimal
  pure { cards: cards, bid: bid }

cardParser :: Parser String Card
cardParser =
  (string "A" $> Ace)
    <|> (string "K" $> King)
    <|> (string "Q" $> Queen)
    <|> (string "J" $> Jack)
    <|> (string "T" $> Ten)
    <|> (string "9" $> Nine)
    <|> (string "8" $> Eight)
    <|> (string "7" $> Seven)
    <|> (string "6" $> Six)
    <|> (string "5" $> Five)
    <|> (string "4" $> Four)
    <|> (string "3" $> Three)
    <|> (string "2" $> Two)
