module Test.Main where

import Prelude
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Main (differences, extrapolate)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "Main" do
          it "should generate list of differences" do
            let
              result = differences (1 : 2 : 4 : Nil)

              result2 = differences (1 : Nil)
            result `shouldEqual` (1 : 2 : Nil)
            result2 `shouldEqual` Nil
          it "Should extrapolate" do
            let
              result = extrapolate (10 : 13 : 16 : 21 : 30 : 45 : Nil)
            result `shouldEqual` (Just (5 /\ 68))
