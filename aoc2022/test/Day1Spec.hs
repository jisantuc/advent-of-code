{-# LANGUAGE OverloadedStrings #-}

module Day1Spec where

import Data.Text (Text)
import Data.Void (Void)
import Day1 (Puzzle, knapsackParser, puzzleParser, solvePart1, solvePart2)
import Parser (parsePuzzle)
import Test.Hspec (Spec, describe, it, shouldBe)
import Testing (expectParsed)
import Text.Megaparsec (ParseErrorBundle)

spec :: Spec
spec = describe "day 1 puzzle" $ do
  it "parses one knapsack" $
    parsePuzzle knapsackParser "1000\n2000\n3000" `shouldBe` Right [1000, 2000, 3000]
  it "parses the example" $
    parsedExample `shouldBe` Right [[1000, 2000, 3000], [4000], [5000, 6000], [7000, 8000, 9000], [10000]]
  it "gets the right answer for the part 1 example" $
    expectParsed parsedExample $
      \puzz -> solvePart1 puzz `shouldBe` 24000
  it "gets the right answer for the part 2 example" $
    expectParsed parsedExample $
      \puzz -> solvePart2 puzz `shouldBe` 45000

parsedExample :: Either (ParseErrorBundle Text Void) Puzzle
parsedExample = parsePuzzle puzzleParser testPuzzle

testPuzzle :: Text
testPuzzle = "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000"
