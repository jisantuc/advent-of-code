{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Day5Spec where

import AoC.Parser.Testing (expectParsed, expectSuccessfulParse)
import Data.Interval (IntervalLit (IntervalLit))
import Data.IntervalIndex (at)
import Data.Text (Text)
import Day5 (Puzzle (..), puzzleParser, solve1, solve2)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (parse)
import Text.RawString.QQ (r)

spec :: Spec
spec =
  let parsedSimplePuzzle = parse puzzleParser "" simplePuzzle
   in describe "Day5Spec" $ do
        describe "parsing" $ do
          it "parses the right number of ranges and ingredients" $
            expectParsed parsedSimplePuzzle $ \(Puzzle {freshRanges, ingredients}) -> do
              length ingredients `shouldBe` 6
              freshRanges `at` 1 `shouldBe` []
              freshRanges `at` 5 `shouldBe` [IntervalLit 3 6]
              freshRanges `at` 8 `shouldBe` []
              freshRanges `at` 11 `shouldBe` [IntervalLit 10 15]
              freshRanges `at` 17 `shouldBe` [IntervalLit 16 21, IntervalLit 12 19]
        it "solves part 1" $
          expectParsed parsedSimplePuzzle $ \puzz -> do
            solve1 puzz `shouldBe` 3
        it "solves part 2" $
          expectParsed parsedSimplePuzzle $ \puzz -> do
            solve2 puzz `shouldBe` 14

simplePuzzle :: Text
simplePuzzle =
  [r|3-5
10-14
16-20
12-18

1
5
8
11
17
32|]
