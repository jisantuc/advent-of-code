{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Day3Spec where

import AoC.Parser.Testing (expectParsed, expectSuccessfulParse)
import Data.Foldable (foldMap')
import qualified Data.Map as Map
import Data.Monoid (Sum (..))
import qualified Data.Text as T
import Day3 (EngineSchematicSymbol (..), Puzzle (..), day3Parser, neightborhoodAround, solver1, solver2, symbolPositionsMap)
import Debug.Trace (traceShow)
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotContain)
import Text.Megaparsec (parse)

spec :: Spec
spec =
  let parsedExamplePuzzle = parse day3Parser "" examplePuzzle
   in describe "day 3" $ do
        describe "parser" $ do
          it "should parse the example puzzle at all" $
            let symbolSum :: [EngineSchematicSymbol] -> Sum Int
                symbolSum =
                  foldMap'
                    ( \case
                        Number v _ _ -> Sum v
                        _ -> mempty
                    )
             in do
                  expectSuccessfulParse parsedExamplePuzzle False
                  expectParsed
                    parsedExamplePuzzle
                    ( \(Puzzle symbols) -> traceShow symbols $ symbolSum symbols `shouldBe` 467 + 114 + 35 + 633 + 617 + 58 + 592 + 755 + 664 + 598
                    )
          it "should ignore dots when trying to find special symbols" $
            expectParsed
              parsedExamplePuzzle
              ( \puzz -> Map.keys (symbolPositionsMap puzz) `shouldNotContain` [(2, 1)]
              )
        describe "part 1 solver" $
          it "gets the right answer for the part 1 example puzzle" $
            expectParsed
              parsedExamplePuzzle
              ( \puzz -> solver1 puzz `shouldBe` 4361
              )
        describe "part 2 solver" $
          it "gets the right answer for the part 2 example puzzle" $
            expectParsed
              parsedExamplePuzzle
              ( \puzz -> solver2 puzz `shouldBe` 467835)
        describe "utility methods" $ do
          describe "neightborhoodAround" $ do
            it "builds the right neighborhood for (0, 0) of width 1" $
              neightborhoodAround (0, 0) 1
                `shouldBe` [ (-1, -1),
                             (-1, 0),
                             (-1, 1),
                             (1, -1),
                             (1, 0),
                             (1, 1),
                             (0, -1),
                             (0, 1)
                           ]
            it "builds the right neighborhood for (1, 1) of width 1" $
              neightborhoodAround (1, 1) 2
                `shouldBe` [ (0, 0),
                             (0, 1),
                             (0, 2),
                             (0, 3),
                             (2, 0),
                             (2, 1),
                             (2, 2),
                             (2, 3),
                             (1, 0),
                             (1, 3)
                           ]

examplePuzzle :: T.Text
examplePuzzle =
  T.unlines
    [ "467..114..",
      "...*......",
      "..35..633.",
      "......#...",
      "617*......",
      ".....+.58.",
      "..592.....",
      "......755.",
      "...$.*....",
      ".664.598.."
    ]
