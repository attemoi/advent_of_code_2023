module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Main (enclosedTiles, findStartPipe, findStartCoords, parsePipes, traversePipeline)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "Main" do
          it "Enclosed tiles, simple case" do
            let
              input =
                """
                ...........
                .S-------7.
                .|F-----7|.
                .||.....||.
                .||.....||.
                .|L-7.F-J|.
                .|..|.|..|.
                .L--J.L--J.
                ...........
                """

              allPipes = parsePipes input

              startCoords = findStartCoords input

              startPipe = findStartPipe startCoords allPipes

              pipesInLoop = traversePipeline startCoords startPipe allPipes
            enclosedTiles pipesInLoop `shouldEqual` 4
          it "Enclosed tiles, complex case" do
            let
              input =
                """
                FF7FSF7F7F7F7F7F---7
                L|LJ||||||||||||F--J
                FL-7LJLJ||||||LJL-77
                F--JF--7||LJLJIF7FJ-
                L---JF-JLJIIIIFJLJJ7
                |F|F-JF---7IIIL7L|7|
                |FFJF7L7F-JF7IIL---7
                7-L-JL7||F7|L7F-7F7|
                L.L7LFJ|||||FJL7||LJ
                L7JLJL-JLJLJL--JLJ.L
                """

              allPipes = parsePipes input

              startCoords = findStartCoords input

              startPipe = findStartPipe startCoords allPipes

              pipesInLoop = traversePipeline startCoords startPipe allPipes
            enclosedTiles pipesInLoop `shouldEqual` 10
