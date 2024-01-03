module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Utils (readLines)

main :: Effect Unit
main = do
  lines <- readLines "02-cube-conundrum/input.txt"
  log $ show $ lines