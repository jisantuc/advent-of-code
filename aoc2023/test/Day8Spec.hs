{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Day8Spec where

import AoC.Parser.Testing (expectParsed)
import qualified Data.Map as Map
import Data.Text (Text)
import Day8 (Instruction (..), Node (..), Puzzle (..), parser, solver1)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (parse)
import Text.RawString.QQ (r)

spec :: Spec
spec =
  let parsedExamplePuzzle = parse parser "" examplePuzzle
      parsedExamplePuzzle2 = parse parser "" secondExamplePuzzle
   in describe "day 8" $ do
        describe "parser" $ do
          it "parses the puzzle at all" $
            expectParsed
              parsedExamplePuzzle
              ( \puzz ->
                  do
                    steps puzz `shouldBe` [GoRight, GoLeft]
                    nodes puzz
                      `shouldBe` Map.fromList
                        [ ("AAA", Node "BBB" "CCC"),
                          ("BBB", Node "DDD" "EEE"),
                          ("CCC", Node "ZZZ" "GGG"),
                          ("DDD", Node "DDD" "DDD"),
                          ("EEE", Node "EEE" "EEE"),
                          ("GGG", Node "GGG" "GGG"),
                          ("ZZZ", Node "ZZZ" "ZZZ")
                        ]
              )
        describe "part 1 solver" $
          it "gets the right answer for the example puzzle" $ do
            expectParsed parsedExamplePuzzle (\puzz -> solver1 puzz "AAA" 0 `shouldBe` 2)
            expectParsed parsedExamplePuzzle2 (\puzz -> solver1 puzz "AAA" 0 `shouldBe` 6)

examplePuzzle :: Text
examplePuzzle =
  [r|RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)|]

secondExamplePuzzle :: Text
secondExamplePuzzle =
  [r|LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)|]
