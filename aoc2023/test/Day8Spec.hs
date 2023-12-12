{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Day8Spec where

import AoC.Parser.Testing (expectParsed, expectSuccessfulParse)
import qualified Data.Map as Map
import Data.Text (Text)
import Day8 (Instruction (..), Node (..), Puzzle (..), findZs, parser, solver1, solver2, getStartingNodes)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (parse)
import Text.RawString.QQ (r)

spec :: Spec
spec =
  let parsedExamplePuzzle = parse parser "" examplePuzzle
      parsedExamplePuzzle2 = parse parser "" secondExamplePuzzle
      parsedExamplePuzzle3 = parse parser "" thirdExamplePuzzle
      parsedExamplePuzzle4 = parse parser "" fourthExamplePuzzle
   in describe "day 8" $ do
        describe "parser" $ do
          it "parses the puzzles at all" $ do
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
            expectSuccessfulParse parsedExamplePuzzle3 False
        describe "part 1 solver" $
          it "gets the right answer for the example puzzle" $ do
            expectParsed parsedExamplePuzzle (\puzz -> solver1 puzz "AAA" 0 `shouldBe` 2)
            expectParsed parsedExamplePuzzle2 (\puzz -> solver1 puzz "AAA" 0 `shouldBe` 6)
        describe "part 2 solver" $ do
          it "gets the right answer for the example puzzle" $ do
            expectParsed
              parsedExamplePuzzle3
              ( \puzz -> do
                  -- search size = 10 * 2
                  solver2 puzz (getStartingNodes puzz) 0 1000 10 `shouldBe` 6
                  -- search size = 3 * 2
                  solver2 puzz (getStartingNodes puzz) 0 1000 3 `shouldBe` 6
                  -- search size = 1 * 2
                  solver2 puzz (getStartingNodes puzz) 0 1000 1 `shouldBe` 6
              )
        describe "findZs" $
          do
            it "finds the right counts for the third example puzzle with different search sizes" $
              do
                expectParsed
                  parsedExamplePuzzle3
                  ( \(Puzzle {nodes, steps}) -> do
                      findZs nodes steps "11A" 10 0 [] `shouldBe` ("11Z", [2, 4, 6, 8, 10])
                      findZs nodes steps "22A" 10 0 [] `shouldBe` ("22B", [3, 6, 9])
                  )
            it "finds the right counts for a fourth example puzzle that doesn't cleanly repeat" $
              -- there's a BCD LLR cycle after the second z at 4
              expectParsed parsedExamplePuzzle4 (\(Puzzle {nodes, steps}) -> findZs nodes steps "44A" 1000 0 [] `shouldBe` ("44D", [2, 4]))

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

thirdExamplePuzzle :: Text
thirdExamplePuzzle =
  [r|LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)|]

fourthExamplePuzzle :: Text
fourthExamplePuzzle =
  [r|LLR

44A = (44B, 44H)
44B = (44Z, 44C)
44C = (44D, 44Z)
44D = (44B, 44Z)
44Z = (44B, 44B)|]
