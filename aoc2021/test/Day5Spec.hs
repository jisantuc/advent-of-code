{-# LANGUAGE OverloadedStrings #-}

module Day5Spec where

import Data.Text (Text)
import Data.Void (Void)
import Day5 (Puzzle, puzzleParser, solvePart1Fast, solvePart2)
import Parser (parsePuzzle)
import Test.Hspec (Spec, describe, it, shouldBe)
import Testing (expectParsed, expectSuccessfulParse)
import Text.Megaparsec (ParseErrorBundle)

spec :: Spec
spec = describe "day 5 puzzle" $ do
  it "should parse at all" $
    expectSuccessfulParse (parsePuzzle puzzleParser testPuzzle) False
  it "should get the right answer for part 1" $ do
    expectParsed parsedExample $ \puzz -> solvePart1Fast puzz `shouldBe` 5
  it "should get the right answer for part 2" $ do
    expectParsed parsedExample $ \puzz -> solvePart2 puzz `shouldBe` 12

parsedExample :: Either (ParseErrorBundle Text Void) Puzzle
parsedExample = parsePuzzle puzzleParser testPuzzle

testPuzzle :: Text
testPuzzle = "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2"
