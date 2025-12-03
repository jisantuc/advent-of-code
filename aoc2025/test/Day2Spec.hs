{-# LANGUAGE OverloadedStrings #-}

module Day2Spec where

import AoC.Parser.Testing (expectParsed, expectSuccessfulParse)
import Data.Interval (IntervalLit (..))
import Data.Text (Text)
import Day2 (puzzleParser, solve1, solve2)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (parse)

spec :: Spec
spec =
  let parsedSimplePuzzle = parse puzzleParser "" simplePuzzle
   in describe "Day2Spec" $ do
        it "parses" $ do
          expectSuccessfulParse parsedSimplePuzzle False
        it "parses all the intervals" $
          expectParsed parsedSimplePuzzle $ \puzz -> do
            take 1 puzz `shouldBe` [IntervalLit 11 22]
            (take 2 . drop 3 $ puzz)
              `shouldBe` [IntervalLit 1188511880 1188511890, IntervalLit 222220 222224]
        it "solves part 1" $ do
          solve1 [IntervalLit 11 22] `shouldBe` 33
          solve1 [IntervalLit 446443 446449] `shouldBe` 446446
          solve1 [IntervalLit 38593856 38593862] `shouldBe` 38593859
          expectParsed parsedSimplePuzzle $ \puzz -> solve1 puzz `shouldBe` 1227775554
        it "solves part 2" $ do
          solve2 [IntervalLit 1 9] `shouldBe` 0
          solve2 [IntervalLit 12 12] `shouldBe` 0
          solve2 [IntervalLit 11 22] `shouldBe` 33
          solve2 [IntervalLit 998 1012] `shouldBe` 999 + 1010
          solve2 [IntervalLit 824824821 824824827] `shouldBe` 824824824
          solve2 [IntervalLit 2121212118 2121212124] `shouldBe` 2121212121
          solve2 [IntervalLit 110 112] `shouldBe` 111
          solve2 [IntervalLit 121211 121213] `shouldBe` 121212
          solve2 [IntervalLit 1698522 1698528] `shouldBe` 0
          expectParsed parsedSimplePuzzle $ \puzz ->
            solve2 puzz `shouldBe` 4174379265

simplePuzzle :: Text
simplePuzzle =
  mconcat
    [ "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,",
      "1698522-1698528,446443-446449,38593856-38593862,565653-565659,",
      "824824821-824824827,2121212118-2121212124"
    ]
