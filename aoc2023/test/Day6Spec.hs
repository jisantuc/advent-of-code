{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Day6Spec where

import AoC.Parser.Testing (expectParsed, expectSuccessfulParse)
import qualified Data.Text as T
import Day6 (Puzzle (..), Race (..), parser, solver1, winningMovesForRace)
import Test.Hspec (Spec, describe, it, shouldBe, it)
import Text.Megaparsec (parse)
import Text.RawString.QQ (r)

spec :: Spec
spec =
  let parsedExamplePuzzle = parse parser "" examplePuzzle
   in describe "day 5" $ do
        describe "parser" $
          it "parses the puzzle at all" $ do
            expectSuccessfulParse parsedExamplePuzzle True
            expectParsed
              parsedExamplePuzzle
              ( \(Puzzle races) ->
                  races `shouldBe` [Race 7 9, Race 15 40, Race 30 200]
              )
        describe "part 1 solver" $ do
          it "finds the right numer of winning choices for the examples" $ do
            winningMovesForRace (Race 7 9) `shouldBe` 4
            winningMovesForRace (Race 15 40) `shouldBe` 8
            winningMovesForRace (Race 30 200) `shouldBe` 9
          it "gets the right answer for the example puzzle" $
            expectParsed
              parsedExamplePuzzle
              ( \puzz ->
                  solver1 puzz `shouldBe` 288
              )
        describe "part 2 solver" $
          it "gets the right answer for the example part 2" $
            winningMovesForRace (Race 71530 940200) `shouldBe` 71503

examplePuzzle :: T.Text
examplePuzzle =
  [r|Time:      7  15   30
Distance:  9  40  200|]
