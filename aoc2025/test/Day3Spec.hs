{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Day3Spec where

import AoC.Parser.Testing (expectSuccessfulParse, expectParsed)
import Data.Text (Text)
import Day3 (puzzleParser, solve1, solve2)
import Test.Hspec (Spec, describe, it, pending, shouldBe)
import Text.Megaparsec (parse)
import Text.RawString.QQ (r)

spec :: Spec
spec =
  let parsedSimplePuzzle = parse puzzleParser "" simplePuzzle
   in describe "Day3Spec" $ do
        it "parses at all" $ expectSuccessfulParse parsedSimplePuzzle False
        it "parses the right shape" $
          expectParsed parsedSimplePuzzle $ \puzz -> do
            length puzz `shouldBe` 4
            length (head puzz) `shouldBe` 15
            length (puzz !! 3) `shouldBe` 15
        it "solves part 1" $ do
          expectParsed parsedSimplePuzzle $ \puzz -> do
            solve1 (take 1 puzz) `shouldBe` 98
            solve1 [puzz !! 1]` shouldBe` 89
            solve1 [puzz !! 2]` shouldBe` 78
            solve1 [puzz !! 3]` shouldBe` 92
            solve1 puzz `shouldBe` 357
        it "solves part 2" $
          expectParsed parsedSimplePuzzle $ \puzz -> do
            solve2 (take 1 puzz) `shouldBe` 987654321111
            solve2 [puzz !! 1]` shouldBe` 811111111119
            solve2 [puzz !! 2]` shouldBe` 434234234278
            solve2 [puzz !! 3]` shouldBe` 888911112111
            solve2 puzz `shouldBe` 3121910778619

simplePuzzle :: Text
simplePuzzle =
  [r|987654321111111
811111111111119
234234234234278
818181911112111|]
