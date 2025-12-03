{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Day3Spec where

import AoC.Parser.Testing (expectSuccessfulParse, expectParsed)
import Data.Text (Text)
import Day3 (puzzleParser, solve1)
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
        it "solves part 1" $
          expectParsed parsedSimplePuzzle $ \puzz ->
            solve1 puzz `shouldBe` 357

simplePuzzle :: Text
simplePuzzle =
  [r|987654321111111
811111111111119
234234234234278
818181911112111|]
