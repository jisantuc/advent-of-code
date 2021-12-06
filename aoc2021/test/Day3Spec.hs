{-# LANGUAGE OverloadedStrings #-}

module Day3Spec where

import Data.Text (Text)
import Data.Void (Void)
import Day3 (puzzleParser, Puzzle, solvePart1, findGammaBit, solvePart2)
import Parser (parsePuzzle)
import Test.Hspec (Spec, describe, it, shouldBe)
import Testing (expectParsed)
import Text.Megaparsec (ParseErrorBundle)

spec :: Spec
spec = describe "day 1 puzzle" $ do
  it "parses the example" $
    parsedExample
      `shouldBe` ( Right [ 4, 30, 22, 23, 21, 15, 7, 28, 16, 25, 2, 10 ]
                 )
  it "gets the right answer for the part 1 example" $
    expectParsed parsedExample $ \puzz -> solvePart1 puzz `shouldBe` 198
  -- need to count down from 4 instead of up from 0 
  it "finds the correct first gamma bit for the example puzzle" $
    expectParsed parsedExample $ \puzz -> findGammaBit puzz 4 `shouldBe` True
  it "finds the correct second gamma bit for the example puzzle" $
    expectParsed parsedExample $ \puzz -> findGammaBit puzz 3 `shouldBe` False
  it "finds all correct bits for the exapmle puzzle" $
    -- 7, 5, 8, 7, 5
    -- It's backwards! :tada:
    expectParsed parsedExample $ \puzz -> findGammaBit puzz <$> reverse [0 .. 4] `shouldBe` [True, False, True, True, False]
  it "gets the right answer for the part 2 example" $
    expectParsed parsedExample $ \puzz -> solvePart2 puzz `shouldBe` Just 230

parsedExample :: Either (ParseErrorBundle Text Void) Puzzle
parsedExample = parsePuzzle puzzleParser testPuzzle

testPuzzle :: Text
testPuzzle = "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010\n"
