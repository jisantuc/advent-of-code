{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Day4Spec where

import Data.Either (isRight)
import Data.Text (Text)
import Data.Void (Void)
import Day4 (ElfPair, puzzleParser, solvePart1, solvePart2)
import Parser (parsePuzzle)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (ParseErrorBundle)
import Text.RawString.QQ (r)
import Testing (expectParsed)

spec :: Spec
spec = describe "day 4 puzzle" $ do
  describe "parsing" $ do
    it "parses the puzzle" $ do
      parsedExample `shouldSatisfy` isRight
      expectParsed parsedExample $ \puzz ->
        length puzz `shouldBe` 6
  describe "solutions" $ do
    it "gets the right answer for the example in part 1" $ do
      expectParsed parsedExample $ \puzz ->
        solvePart1 puzz `shouldBe` 2
    it "gets the right answer for the example in part 2" $ do
      expectParsed parsedExample $ \puzz ->
        solvePart2 puzz `shouldBe` 4

parsedExample :: Either (ParseErrorBundle Text Void) [ElfPair]
parsedExample = parsePuzzle puzzleParser examplePuzzle

examplePuzzle :: Text
examplePuzzle =
  [r|2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8
|]
