{-# LANGUAGE OverloadedStrings #-}

module Day1Spec where

import Data.Text (Text)
import Data.Void (Void)
import Day1 (Puzzle, puzzleParser, solvePart1, solvePart2)
import Parser (parsePuzzle)
import Test.Hspec (Spec, describe, it, shouldBe)
import Testing (expectParsed)
import Text.Megaparsec (ParseErrorBundle)

spec :: Spec
spec = describe "day 1 puzzle" $ do
  it "gets the right answer for the part 1 example" $
    expectParsed parsedExample $ \puzz -> solvePart1 puzz `shouldBe` 7
  it "gets the right answer for the part 2 example" $
    expectParsed parsedExample $ \puzz -> solvePart2 puzz `shouldBe` 5
  it "parses the example" $
    parsedExample `shouldBe` (Right [199, 200, 208, 210, 200, 207, 240, 269, 260, 263])

parsedExample :: Either (ParseErrorBundle Text Void) Puzzle
parsedExample = parsePuzzle puzzleParser testPuzzle

testPuzzle :: Text
testPuzzle = "199\n200\n208\n210\n200\n207\n240\n269\n260\n263\n"
