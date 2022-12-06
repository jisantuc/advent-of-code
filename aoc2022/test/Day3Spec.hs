{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Day3Spec where

import Data.Either (isRight)
import Data.Foldable (traverse_)
import Data.Monoid (Sum (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Day3 (ElfTeam, Priority (..), Rucksack, priority, puzzleParser, puzzleParser2, rucksackParser, scoreRucksack, solvePart1, solvePart2)
import Debug.Trace (traceShowId)
import Parser (parsePuzzle)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe, shouldSatisfy)
import Testing (expectParsed)
import Text.Megaparsec (ParseErrorBundle)
import Text.RawString.QQ (r)

spec :: Spec
spec = describe "day 3 puzzle" $ do
  describe "parsing" $ do
    it "parses the puzzle -- part1" $ do
      parsedExample `shouldSatisfy` isRight
      expectParsed parsedExample $ \puzz -> length puzz `shouldBe` 6
    it "parses the puzzle -- part2" $ do
      parsedExample2 `shouldSatisfy` isRight
      expectParsed parsedExample2 $ \puzz -> length puzz `shouldBe` 2
  describe "part 1 solution" $ do
    it "solves the example puzzle" $
      expectParsed parsedExample $
        \puzz -> solvePart1 puzz `shouldBe` 157
    it "scores each subcomponent of part 1 correctly" $
      let puzzLines = T.lines testPuzzle
          expectedScores = Priority . Sum <$> [16, 38, 42, 22, 20, 19]
          lineScorePairs = zip puzzLines expectedScores
       in traverse_
            ( \(puzzLine, score) ->
                expectParsed (parsePuzzle rucksackParser puzzLine) $ \l ->
                  scoreRucksack (traceShowId l) `shouldBe` score
            )
            lineScorePairs
    it "prioritizes chars from the example correctly" $ do
      priorityTest 'p' 16
      priorityTest 'L' 38
      priorityTest 'P' 42
      priorityTest 'v' 22
      priorityTest 't' 20
      priorityTest 's' 19
  describe "part 2 solution" $ do
    it "solves the example puzzle" $
      expectParsed parsedExample2 $
        \puzz -> solvePart2 puzz `shouldBe` 70

parsedExample :: Either (ParseErrorBundle Text Void) [Rucksack]
parsedExample = parsePuzzle puzzleParser testPuzzle

parsedExample2 :: Either (ParseErrorBundle Text Void) [ElfTeam]
parsedExample2 = parsePuzzle puzzleParser2 testPuzzle

testPuzzle :: Text
testPuzzle =
  [r|vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
|]

priorityTest :: Char -> Int -> Expectation
priorityTest c e = priority c `shouldBe` (Priority . Sum $ e)
