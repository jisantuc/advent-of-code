{-# LANGUAGE OverloadedStrings #-}

module Day9Spec where

import AoC.Parser.Testing (expectParsed, expectSuccessfulParse)
import Data.Text (Text)
import Day9 (Disk (..), puzzleParser, solve1, solve2)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (parse)

spec :: Spec
spec =
  describe "Day9Spec" $
    let parsedExamplePuzzle = parse puzzleParser "" examplePuzzle
     in do
          it "parses" $ do
            expectSuccessfulParse parsedExamplePuzzle False
            expectParsed parsedExamplePuzzle $ \puzz ->
              take 5 puzz `shouldBe` [File 0 2, EmptySpace 3, File 1 3, EmptySpace 3, File 2 1]
          describe "day1" $
            it "solves" $
              expectParsed parsedExamplePuzzle $ \puzz ->
                solve1 puzz `shouldBe` 1928
          describe "day2" $
            it "solves" $
              expectParsed parsedExamplePuzzle $ \puzz ->
                solve2 puzz `shouldBe` 2858

examplePuzzle :: Text
examplePuzzle = "2333133121414131402"
