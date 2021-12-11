{-# LANGUAGE OverloadedStrings #-}

module Day5Spec where

import Data.Text (Text)
import Data.Void (Void)
import Day5 (Location (..), Puzzle, Segment (..), puzzleParser, reachableFrom, solvePart1)
import Parser (parsePuzzle)
import Test.Hspec (Spec, describe, it, shouldBe)
import Testing (expectParsed, expectSuccessfulParse)
import Text.Megaparsec (ParseErrorBundle)

spec :: Spec
spec = describe "day 5 puzzle" $ do
  it "should parse at all" $
    expectSuccessfulParse (parsePuzzle puzzleParser testPuzzle) False
  it "should agree about containment for some simple cases" $ do
    reachableFrom (Location 0 0) (Segment (Location 0 0) (Location 0 0)) `shouldBe` True
    reachableFrom (Location 1 1) (Segment (Location 1 0) (Location 1 2)) `shouldBe` True
    reachableFrom (Location 1 1) (Segment (Location 0 1) (Location 2 1)) `shouldBe` True
    -- ignoring diagonals
    reachableFrom (Location 1 1) (Segment (Location 0 0) (Location 2 2)) `shouldBe` False
  it "should agree about non-containment for some simple cases" $ do
    reachableFrom (Location 0 0) (Segment (Location 1 1) (Location 2 2)) `shouldBe` False
    reachableFrom (Location 1 1) (Segment (Location 0 0) (Location 2 1)) `shouldBe` False
  it "should get the right answer for part 1" $ do
    expectParsed parsedExample $ \puzz -> solvePart1 puzz `shouldBe` 5

parsedExample :: Either (ParseErrorBundle Text Void) Puzzle
parsedExample = parsePuzzle puzzleParser testPuzzle

testPuzzle :: Text
testPuzzle = "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2"
