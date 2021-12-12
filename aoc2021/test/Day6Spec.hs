{-# LANGUAGE OverloadedStrings #-}

module Day6Spec where

import Data.Text (Text)
import Data.Void (Void)
import Day6 (Puzzle, puzzleParser, solvePart1, solvePart2)
import Parser (parsePuzzle)
import Test.Hspec (Spec, describe, it, shouldBe)
import Testing (expectParsed, expectSuccessfulParse)
import Text.Megaparsec (ParseErrorBundle)

spec :: Spec
spec = describe "day 6 puzzle" $ do
  it "should parse at all" $
    expectSuccessfulParse (parsePuzzle puzzleParser testPuzzle) False
  it "should get the right answer for part 1" $
    expectParsed parsedExample $ \puzz -> solvePart1 puzz `shouldBe` 5934
  it "should get the right answer for part 2" $
    expectParsed parsedExample $ \puzz -> solvePart2 puzz `shouldBe` 26984457539

parsedExample :: Either (ParseErrorBundle Text Void) Puzzle
parsedExample = parsePuzzle puzzleParser testPuzzle

testPuzzle :: Text
testPuzzle = "3,4,3,1,2\n"
