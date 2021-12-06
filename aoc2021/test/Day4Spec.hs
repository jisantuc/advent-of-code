{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Day4Spec where

import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Void (Void)
import Day4 (BingoBoard (..), GameState (..), Puzzle (..), boardParser, evaluateGame, findUnmarkedScore, puzzleParser, rowParser, solvePart1)
import Parser (parsePuzzle)
import Test.Hspec (Spec, describe, it, shouldBe)
import Testing (expectParsed, expectParsedIO, expectSuccessfulParse)
import Text.Megaparsec (ParseErrorBundle)

-- need to add quasiquotes for the test here because input
-- is considerably more complicated
-- then test that:
--   test input produces three puzzles on parse
--   no winners are produced after the first five numbers
--   for any given board, if `lookup v board` returns coords,
--     then calling `mark v` then `checkSquare` on the returned coords
--     returns True (this is _extremely_ property testable!)

spec :: Spec
spec = describe "day 4 puzzle" $ do
  it "should parse a single board" $
    expectSuccessfulParse (parsePuzzle boardParser testBoard) False
  it "should have all the rows for a single board" $
    expectParsed (parsePuzzle boardParser testBoard) $ \bingoBoard ->
      length (board bingoBoard) `shouldBe` 25
  it "should parse a single row" $
    expectSuccessfulParse (parsePuzzle rowParser testRow) False
  it "should parse the puzzle" $
    parsedExample >>= (\x -> expectSuccessfulParse x False)
  it "should read the right number of bingo boards" $
    expectParsedIO parsedExample $ \(Puzzle {boards}) -> length boards `shouldBe` 3
  it "should get the right unmarked sum for part 1" $
    expectParsedIO parsedExample $ \puzz ->
      let GameState {winner} = evaluateGame puzz
       in case winner of
            Nothing -> fail "No winner was found"
            Just (b, _) -> findUnmarkedScore b `shouldBe` 188
  it "should get the right answer for part 1" $
    expectParsedIO parsedExample $ \puzz -> solvePart1 puzz `shouldBe` 4512

parsedExample :: IO (Either (ParseErrorBundle Text Void) Puzzle)
parsedExample = parsePuzzle puzzleParser <$> testPuzzle

testPuzzle :: IO Text
testPuzzle = T.readFile "./test/testPuzzle04.txt"

testBoard :: Text
testBoard = "22 13 17 11  0\n 8  2 23  4 24\n21  9 14 16  7\n 6 10  3 18  5\n 1 12 20 15 19\n"

testRow :: Text
testRow = "22 13 17 11  0\n"
