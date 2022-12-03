{-# LANGUAGE OverloadedStrings #-}

module Day2Spec where

import Data.Text (Text)
import Data.Void (Void)
import Day2 (Round (..), Throw (..), parseRound, puzzleParser1, solvePart1, scoreResult1, puzzleParser2, Round2, solvePart2, scoreResult2, parseRound2)
import Parser (parsePuzzle)
import Test.Hspec (Spec, describe, it, shouldBe)
import Testing (expectParsed)
import Text.Megaparsec (ParseErrorBundle)

spec :: Spec
spec = describe "day 2 puzzle" $ do
  describe "parsing" $ do
    it "parses one round" $
      parsePuzzle parseRound "A Y" `shouldBe` Right (Round Rock Paper)
    it "parses the example" $
      parsedExample `shouldBe` Right [Round Rock Paper, Round Paper Rock, Round Scissors Scissors]
  describe "round scoring" $ do
    it "scores all three example rounds correctly for part 1" $ do
      expectParsed (parsePuzzle parseRound "A Y") $ \puzzRound -> scoreResult1 puzzRound `shouldBe` 6
      expectParsed (parsePuzzle parseRound "B X") $ \puzzRound -> scoreResult1 puzzRound `shouldBe` 0
      expectParsed (parsePuzzle parseRound "C Z") $ \puzzRound -> scoreResult1 puzzRound `shouldBe` 3
    it "scores all three example rounds correctly for part 2" $ do
      expectParsed (parsePuzzle parseRound2 "A Y") $ \puzzRound -> scoreResult2 puzzRound `shouldBe` 4
      expectParsed (parsePuzzle parseRound2 "B X") $ \puzzRound -> scoreResult2 puzzRound `shouldBe` 1
      expectParsed (parsePuzzle parseRound2 "C Z") $ \puzzRound -> scoreResult2 puzzRound `shouldBe` 7
  describe "solutions" $ do
    it "gets the right answer for the part 1 example" $
      expectParsed parsedExample $
        \puzz -> solvePart1 puzz `shouldBe` 15
    it "gets the right ansewr for the part 2 example" $ do
      expectParsed parsedExample2 $ \puzz ->
        solvePart2 puzz `shouldBe` 12

parsedExample :: Either (ParseErrorBundle Text Void) [Round]
parsedExample = parsePuzzle puzzleParser1 testPuzzle

parsedExample2 :: Either (ParseErrorBundle Text Void) [Round2]
parsedExample2 = parsePuzzle puzzleParser2 testPuzzle

testPuzzle :: Text
testPuzzle = "A Y\nB X\nC Z\n"
