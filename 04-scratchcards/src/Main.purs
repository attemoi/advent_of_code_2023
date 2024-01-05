module Main where

import Prelude
import Data.Array (foldl, mapMaybe, slice)
import Data.Array as Array
import Data.Either (hush)
import Data.Foldable (sum)
import Data.Int (pow)
import Data.List (List, intersect, length)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Console (log)
import Parsing (Parser, runParser)
import Parsing.Combinators (many, manyTill)
import Parsing.String (string)
import Parsing.String.Basic (intDecimal, skipSpaces)
import Utils (readLines)

type Card
  = { id :: Int, winning :: List Int, scratched :: List Int }

main :: Effect Unit
main = do
  lines <- readLines "04-scratchcards/input.txt"
  let
    cards = mapMaybe parseCard lines
  log $ show $ "Part 1, total points for all cards: "
    <> (show $ sum $ map cardPoints cards)
  let
    cardsWon = sum $ map (\card -> numCardsWon cards card) cards
  log $ show $ "Part 2, number of cards won: "
    <> (show $ cardsWon + (Array.length cards))

parseCard :: String -> Maybe Card
parseCard input = hush $ runParser input cardParser

winningNumbers :: Card -> List Int
winningNumbers card = intersect card.winning card.scratched

cardPoints :: Card -> Int
cardPoints card = points (length $ winningNumbers card)

numCardsWon :: Array Card -> Card -> Int
numCardsWon allCards card =
  let
    cardsWon = length $ winningNumbers card

    nextCards = slice (card.id) (card.id + cardsWon) allCards
  in
    cardsWon + (foldl (\acc card' -> acc + numCardsWon allCards card') 0 nextCards)

points :: Int -> Int
points 0 = 0

points numOfWinningCards = 2 `pow` (numOfWinningCards - 1)

cardParser :: Parser String Card
cardParser = do
  _ <- string "Card"
  skipSpaces
  id <- intDecimal
  _ <- string ":"
  skipSpaces
  winning <- manyTill numberWithSpacesParser (string "|")
  skipSpaces
  scratched <- many numberWithSpacesParser
  pure ({ id: id, winning: winning, scratched: scratched })

numberWithSpacesParser :: Parser String Int
numberWithSpacesParser = do
  number <- intDecimal
  skipSpaces
  pure number
