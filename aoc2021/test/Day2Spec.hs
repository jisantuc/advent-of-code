{-# LANGUAGE OverloadedStrings #-}

module Day2Spec where

import Data.Text (Text)
import Data.Void (Void)
import Day2 (Instruction (..), Puzzle, puzzleParser, solvePart1, solvePart2)
import Parser (parsePuzzle)
import Test.Hspec (Spec, describe, it, shouldBe)
import Testing (expectParsed)
import Text.Megaparsec (ParseErrorBundle)

spec :: Spec
spec = describe "day 1 puzzle" $ do
  it "parses the example" $
    parsedExample
      `shouldBe` ( Right
                     [ Horizontal 5,
                       Vertical 5,
                       Horizontal 8,
                       Vertical (negate 3),
                       Vertical 8,
                       Horizontal 2
                     ]
                 )
  it "gets the right answer for the part 1 example" $
    expectParsed parsedExample $ \puzz -> solvePart1 puzz `shouldBe` 150
  it "gets the right answer for the part 2 example" $
    expectParsed parsedExample $ \puzz -> solvePart2 puzz `shouldBe` 900

parsedExample :: Either (ParseErrorBundle Text Void) Puzzle
parsedExample = parsePuzzle puzzleParser testPuzzle

testPuzzle :: Text
testPuzzle = "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2\n"
