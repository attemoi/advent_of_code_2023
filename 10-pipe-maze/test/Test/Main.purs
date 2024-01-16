module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Main (enclosedTiles, parsePipes, traversePipeline, findStartPipe)
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

              startPipe = findStartPipe input allPipes

              pipesInLoop = traversePipeline startPipe allPipes
            enclosedTiles pipesInLoop `shouldEqual` 4
          it "Enclosed tiles, medium case" do
            let
              input =
                """
                .S----7F7F7F7F-7....
                .|F--7||||||||FJ....
                .||.FJ||||||||L7....
                FJL7L7LJLJ||LJ.L-7..
                L--J.L7...LJF7F-7L7.
                ....F-J..F7FJ|L7L7L7
                ....L7.F7||L7|.L7L7|
                .....|FJLJ|FJ|F7|.LJ
                ....FJL-7.||.||||...
                ....L---J.LJ.LJLJ...
                """

              allPipes = parsePipes input

              startPipe = findStartPipe input allPipes

              pipesInLoop = traversePipeline startPipe allPipes
            enclosedTiles pipesInLoop `shouldEqual` 8
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

              startPipe = findStartPipe input allPipes

              pipesInLoop = traversePipeline startPipe allPipes
            enclosedTiles pipesInLoop `shouldEqual` 10
