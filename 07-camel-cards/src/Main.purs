module Main where

import Prelude
import Control.Alt ((<|>))
import Data.Array (mapMaybe, sort)
import Data.Either (hush)
import Data.Foldable (any, foldl, length)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.Map (Map, empty, filter, insertWith, values)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import Parsing (Parser, runParser)
import Parsing.Combinators (many)
import Parsing.String (string)
import Parsing.String.Basic (intDecimal, skipSpaces)
import Utils (readLines)

newtype Hand
  = Hand { cards :: List Card, bid :: Int }

handCards :: Hand -> List Card
handCards (Hand h) = _.cards h

handBid :: Hand -> Int
handBid (Hand h) = _.bid h

derive newtype instance eqHand :: Eq Hand

instance ordHand :: Ord Hand where
  compare a b
    | (handType $ handCards a) == (handType $ handCards b) = compareHighCards (handCards a) (handCards b)
    | otherwise = compare (handType $ handCards a) (handType $ handCards b)

compareHighCards :: List Card -> List Card -> Ordering
compareHighCards Nil _ = LT

compareHighCards _ Nil = GT

compareHighCards (a : as) (b : bs)
  | a == b = compareHighCards as bs
  | otherwise = compare a b

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
    hands = sort $ mapMaybe parseHand inputLines

    totalWinnings =
      snd
        $ foldl
            ( \(i /\ acc) hand ->
                (i + 1) /\ (acc + (handBid hand) * (i + 1))
            )
            (0 /\ 0)
            hands
  log $ show $ "Part1 (total winnings): " <> (show $ totalWinnings)

parseHand :: String -> Maybe Hand
parseHand input = hush $ runParser input handParser

handParser :: Parser String Hand
handParser = do
  cards' <- many cardParser
  skipSpaces
  bid' <- intDecimal
  pure (Hand { cards: cards', bid: bid' })

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

handType :: List Card -> HandType
handType cards
  | nCards 5 cards = FiveOfKind
  | nCards 4 cards = FourOfKind
  | nCards 3 cards && nCards 2 cards = FullHouse
  | nCards 3 cards = ThreeOfKind
  | length (filter (\count -> count == 2) (cardCounts cards)) == 2 = TwoPairs
  | nCards 2 cards = OnePair
  | otherwise = HighCard

nCards :: Int -> List Card -> Boolean
nCards n cards = any (\count -> count == n) (values $ cardCounts cards)

cardCounts :: List Card -> Map Card Int
cardCounts cards = foldl (\map card -> insertWith (+) card 1 map) empty cards
