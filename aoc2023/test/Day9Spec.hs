{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Day9Spec where

import AoC.Parser.Testing (expectParsed)
import Data.Text (Text)
import Day9 (Puzzle (..), findNextValue, parser, solver1, solver2)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (parse)
import Text.RawString.QQ (r)

spec :: Spec
spec =
  let parsedExamplePuzzle = parse parser "" examplePuzzle
   in describe "day 9" $ do
        describe "parser" $ do
          it "parses the example at all" $
            expectParsed
              parsedExamplePuzzle
              ( \(Puzzle lists) ->
                  lists
                    `shouldBe` [ [0, 3, 6, 9, 12, 15],
                                 [1, 3, 6, 10, 15, 21],
                                 [10, 13, 16, 21, 30, 45]
                               ]
              )
          it "parses an example with a negative value" $
            expectParsed
              (parse parser "" examplePuzzle2)
              ( \(Puzzle lists) ->
                  lists `shouldBe` [[-1, 1, 3, 5]]
              )
        describe "next value finder" $ do
          it "finds the right next values for each part 1 component" $ do
            expectParsed
              parsedExamplePuzzle
              ( \(Puzzle lists) -> do
                  findNextValue (head lists) `shouldBe` 18
                  findNextValue (lists !! 1) `shouldBe` 28
                  findNextValue (lists !! 2) `shouldBe` 68
              )
          it "works in reverse" $ do
            expectParsed
              parsedExamplePuzzle
              ( \(Puzzle lists) -> do
                  findNextValue (reverse . head $ lists) `shouldBe` negate 3
                  findNextValue (reverse . (!! 1) $ lists) `shouldBe` 0
                  findNextValue (reverse . (!! 2) $ lists) `shouldBe` 5
              )
        describe "part 1 solver" $ do
          it "finds the right answer for the example puzzle" $ do
            expectParsed parsedExamplePuzzle (\puzz -> solver1 puzz `shouldBe` 114)
        describe "part 2 solver" $ do
          it "finds the right answer for the example puzzle" $ do
            expectParsed parsedExamplePuzzle (\puzz -> solver2 puzz `shouldBe` 2)

examplePuzzle :: Text
examplePuzzle =
  [r|0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45|]

examplePuzzle2 :: Text
examplePuzzle2 =
  [r|-1 1 3 5|]
