module Main where

import Prelude
import Control.Alt ((<|>))
import Data.Array (mapMaybe)
import Data.Array as Array
import Data.Either (hush)
import Data.Foldable (all, foldl)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), modifyAt, reverse, sort, (:))
import Data.Map (empty, insertWith, pop, values)
import Data.Maybe (Maybe, fromMaybe)
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
  = Joker
  | Two
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

data JRule
  = IsJack
  | IsJoker

derive instance eqHandType :: Eq HandType

derive instance ordHandType :: Ord HandType

derive instance genericHandType :: Generic HandType _

instance showHandType :: Show HandType where
  show = genericShow

main :: Effect Unit
main = do
  inputLines <- readLines "07-camel-cards/input.txt"
  let
    hands = Array.sort $ mapMaybe (parseHand IsJack) inputLines

    handsWithJoker = Array.sort $ mapMaybe (parseHand IsJoker) inputLines
  log $ show $ "Part1 (total winnings): " <> (show $ winnings hands)
  log $ show $ "Part2 (total winnings with jokers): " <> (show $ winnings handsWithJoker)

winnings :: Array Hand -> Int
winnings hands =
  snd
    $ foldl
        ( \(i /\ acc) hand ->
            (i + 1) /\ (acc + (handBid hand) * (i + 1))
        )
        (0 /\ 0)
        hands

parseHand :: JRule -> String -> Maybe Hand
parseHand j input = hush $ runParser input (handParser j)

handParser :: JRule -> Parser String Hand
handParser jCard = do
  cards' <- many (cardParser jCard)
  skipSpaces
  bid' <- intDecimal
  pure (Hand { cards: cards', bid: bid' })

cardParser :: JRule -> Parser String Card
cardParser jRule =
  (string "A" $> Ace)
    <|> (string "K" $> King)
    <|> (string "Q" $> Queen)
    <|> jCardParser jRule
    <|> (string "T" $> Ten)
    <|> (string "9" $> Nine)
    <|> (string "8" $> Eight)
    <|> (string "7" $> Seven)
    <|> (string "6" $> Six)
    <|> (string "5" $> Five)
    <|> (string "4" $> Four)
    <|> (string "3" $> Three)
    <|> (string "2" $> Two)

jCardParser :: JRule -> Parser String Card
jCardParser IsJack = string "J" $> Jack

jCardParser IsJoker = string "J" $> Joker

handType :: List Card -> HandType
handType cards
  | all isJoker cards = FiveOfKind

handType cards = case cardCounts cards of
  (5 : _) -> FiveOfKind
  (4 : _) -> FourOfKind
  (3 : 2 : _) -> FullHouse
  (3 : _) -> ThreeOfKind
  (2 : 2 : _) -> TwoPairs
  (2 : _) -> OnePair
  _ -> HighCard

isJoker :: Card -> Boolean
isJoker Joker = true

isJoker _ = false

cardCounts :: List Card -> List Int
cardCounts cards =
  let
    counts = foldl (\map card -> insertWith (+) card 1 map) empty cards

    (jokerCount /\ countsWithoutJoker) = fromMaybe (0 /\ counts) (pop Joker counts)

    countsSorted = reverse $ sort $ values countsWithoutJoker

    jokersAddedToBiggestCount = modifyAt 0 (_ + jokerCount) countsSorted
  in
    fromMaybe Nil jokersAddedToBiggestCount
