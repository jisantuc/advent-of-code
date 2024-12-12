{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Day8Spec where

import AoC.Parser.Testing (expectParsed, expectSuccessfulParse)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Day8 (antinodesForPair, puzzleParser, solve1, solve2)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (parse)
import Text.RawString.QQ (r)

spec :: Spec
spec =
  describe "Day8Spec" $
    let parsedExamplePuzzle = parse (puzzleParser (0, 0) mempty) "" examplePuzzle
     in do
          it "parses" $ do
            expectSuccessfulParse parsedExamplePuzzle False
            expectParsed parsedExamplePuzzle $ \((maxRow, maxCol), antennas) -> do
              maxRow `shouldBe` 11
              maxCol `shouldBe` 11
              Set.fromList (Map.keys antennas) `shouldBe` Set.fromList ['0', 'A']
              Map.lookup '0' antennas `shouldBe` Just (Set.fromList [(1, 8), (2, 5), (3, 7), (4, 4)])
              Map.lookup 'A' antennas `shouldBe` Just (Set.fromList [(5, 6), (8, 8), (9, 9)])
          describe "part 1" $ do
            it "finds antinodes for pairs" $ do
              antinodesForPair (3, 5) (5, 6) `shouldBe` [(1, 4), (7, 7)]
            it "solves" $ do
              expectParsed parsedExamplePuzzle $ \(maxBounds, antennas) ->
                solve1 maxBounds antennas `shouldBe` 14
          describe "part 2" $
            it "solves" $
              expectParsed parsedExamplePuzzle $ \(maxBounds, antennas) ->
                solve2 maxBounds antennas `shouldBe` 34

examplePuzzle :: Text
examplePuzzle =
  [r|............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............|]
