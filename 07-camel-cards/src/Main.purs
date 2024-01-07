module Main where

import Prelude
import Control.Alt ((<|>))
import Data.Array (foldl, mapMaybe)
import Data.Either (hush)
import Data.Foldable (any, length)
import Data.Generic.Rep (class Generic)
import Data.Map (Map, empty, filter, insertWith, values)
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

derive instance genericHandType :: Generic HandType _

instance showHandType :: Show HandType where
  show = genericShow

main :: Effect Unit
main = do
  inputLines <- readLines "07-camel-cards/input.txt"
  let
    hands = mapMaybe parseHand inputLines
  log $ show $ map handType $ map _.cards $ hands

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

handType :: Array Card -> HandType
handType cards
  | nCards 5 cards = FiveOfKind
  | nCards 4 cards = FourOfKind
  | nCards 3 cards && nCards 2 cards = FullHouse
  | nCards 3 cards = ThreeOfKind
  | length (filter (\count -> count == 2) (cardCounts cards)) == 2 = TwoPairs
  | nCards 2 cards = OnePair
  | otherwise = HighCard

nCards :: Int -> Array Card -> Boolean
nCards n cards = any (\count -> count == n) (values $ cardCounts cards)

cardCounts :: Array Card -> Map Card Int
cardCounts cards = foldl (\map card -> insertWith (+) card 1 map) empty cards
