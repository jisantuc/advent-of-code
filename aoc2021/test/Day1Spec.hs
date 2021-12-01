{-# LANGUAGE OverloadedStrings #-}

module Day1Spec where

import Day1 (puzzleParser, Puzzle, solvePart2, solvePart1)
import Test.Hspec (Spec, describe, it, shouldBe)
import Data.Text (Text)
import Parser (parsePuzzle)
import Text.Megaparsec (ParseErrorBundle)
import Data.Void (Void)

spec :: Spec
spec = describe "day 1 puzzle" $ do
  it "gets the right answer for the part 1 example" $
    case parsedExample of
      Right puzz -> solvePart1 puzz `shouldBe` 7
      Left _ -> fail "Parsing failed, see the parses the example test for more info"
  it "gets the right answer for the part 2 example" $
    case parsedExample of
      Right puzz -> solvePart2 puzz `shouldBe` 5
      Left _ -> fail "Parsing failed, see the parses the example test for more info"
  it "parses the example" $
    parsedExample `shouldBe` (Right [199, 200, 208, 210, 200, 207, 240, 269, 260, 263])

parsedExample :: Either (ParseErrorBundle Text Void) Puzzle
parsedExample = parsePuzzle puzzleParser testPuzzle

testPuzzle :: Text
testPuzzle = "199\n200\n208\n210\n200\n207\n240\n269\n260\n263\n"
