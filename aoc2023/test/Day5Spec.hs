{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Day5Spec where

import AoC.Parser.Testing (expectParsed, expectSuccessfulParse)
import qualified Data.Text as T
import Day5
  ( Puzzle (..),
    RangeMap (..),
    elemFrom,
    elemTo,
    fertilizerForSoil,
    humidityForTemperature,
    invertRangeMap,
    lightForWater,
    locationForHumidity,
    locationForSeed,
    parser,
    seedsParser1,
    seedsParser2,
    soilForSeed,
    solver1,
    solver2,
    temperatureForLight,
    waterForFertilizer,
  )
import Test.Hspec (Spec, describe, it, shouldBe, xit)
import Text.Megaparsec (parse)
import Text.RawString.QQ (r)

spec :: Spec
spec =
  let parsedExamplePuzzle = parse (parser seedsParser1) "" examplePuzzle
      parsedExamplePuzzle2 = parse (parser seedsParser2) "" examplePuzzle
   in describe "day 5" $ do
        describe "parser" $ do
          it "parses the example puzzle" $ do
            expectSuccessfulParse parsedExamplePuzzle False
          it "parses the example puzzle the part 2 way" $ do
            expectSuccessfulParse parsedExamplePuzzle2 False
            expectParsed
              parsedExamplePuzzle2
              ( \(Puzzle {seedsToPlant}) ->
                  -- there are two pairs in the example puzzle
                  length seedsToPlant `shouldBe` 2
              )
        describe "range map" $ do
          it "finds elements correctly" $ do
            elemTo 98 (RangeMap 98 50 2) `shouldBe` Just 50
            elemTo 99 (RangeMap 98 50 2) `shouldBe` Just 51
            elemTo 94 (RangeMap 95 41 100) `shouldBe` Nothing
            elemTo 42 (RangeMap 38 24 3) `shouldBe` Nothing
          it "inverts correctly" $ do
            let rangeMap = RangeMap {sourceStart = 98, destinationStart = 50, mappingLength = 2}
             in do
                  elemFrom 50 rangeMap `shouldBe` Just 98
                  elemFrom 51 rangeMap `shouldBe` Just 99
                  elemFrom 98 rangeMap `shouldBe` elemTo 98 (invertRangeMap rangeMap)
                  elemFrom 99 rangeMap `shouldBe` elemTo 99 (invertRangeMap rangeMap)
                  elemFrom 50 rangeMap `shouldBe` elemTo 50 (invertRangeMap rangeMap)
                  elemFrom 41 rangeMap `shouldBe` elemTo 41 (invertRangeMap rangeMap)
        describe "part 1 solver" $ do
          it "finds the right complete conversions for the example seeds" $
            expectParsed parsedExamplePuzzle (\puzz -> locationForSeed puzz 79 `shouldBe` 82)
          it "finds the right partial conversions" $
            expectParsed
              parsedExamplePuzzle
              ( \puzz -> do
                  soilForSeed puzz 79 `shouldBe` 81
                  fertilizerForSoil puzz 81 `shouldBe` 81
                  waterForFertilizer puzz 81 `shouldBe` 81
                  lightForWater puzz 81 `shouldBe` 74
                  temperatureForLight puzz 74 `shouldBe` 78
                  humidityForTemperature puzz 78 `shouldBe` 78
                  locationForHumidity puzz 78 `shouldBe` 82
                  locationForSeed puzz 79 `shouldBe` locationForHumidity puzz 78
                  locationForSeed puzz 55 `shouldBe` 86
              )
          it "finds the right answer for the example puzzle" $ do
            expectParsed parsedExamplePuzzle (\puzz -> solver1 puzz `shouldBe` 35)
        describe "part 2 solver" $ do
          xit "finds the right answer for the example puzzle" $ do
            expectParsed parsedExamplePuzzle2 (\puzz -> solver2 puzz `shouldBe` 46)

examplePuzzle :: T.Text
examplePuzzle =
  [r|seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4|]
