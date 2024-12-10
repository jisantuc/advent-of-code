{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Day7Spec where

import AoC.Parser.Testing (expectParsed, expectSuccessfulParse)
import Data.Text (Text)
import Day7 (Operator (..), PuzzleLine (..), findEquation, puzzleParser, solve1, solve2)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (parse)
import Text.RawString.QQ (r)

spec :: Spec
spec = describe "Day7Spec" $ do
  it "parses" $
    let parsedExamplePuzzle = parse puzzleParser "" samplePuzzle
     in do
          expectSuccessfulParse parsedExamplePuzzle False
          expectParsed parsedExamplePuzzle $ \puzz -> do
            length puzz `shouldBe` 9
            (take 5 . (answer <$>) $ puzz) `shouldBe` [190, 3267, 83, 156, 7290]
            (take 2 . (operands <$>) $ puzz) `shouldBe` [[10, 19], [81, 40, 27]]
  describe "part 1" $ do
    it "finds operators for lines" $ do
      findEquation (PuzzleLine 190 [10, 19]) `shouldBe` Just [Multiply]
      findEquation (PuzzleLine 3267 [81, 40, 27]) `shouldBe` Just [Add, Multiply]
      findEquation (PuzzleLine 292 [11, 6, 16, 20]) `shouldBe` Just [Add, Multiply, Add]
    it "solves" $
      let parsedExamplePuzzle = parse puzzleParser "" samplePuzzle
       in expectParsed parsedExamplePuzzle $ \puzz -> solve1 puzz `shouldBe` 3749
  describe "part 2" $
    it "solves" $ 
      let parsedExamplePuzzle = parse puzzleParser "" samplePuzzle
       in expectParsed parsedExamplePuzzle $ \puzz -> solve2 puzz `shouldBe` 11387

samplePuzzle :: Text
samplePuzzle =
  [r|190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20|]
