{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Day4Spec where

import AoC.Data.Grid.Rectangular (RectangularGrid (RectangularGrid))
import AoC.Parser.Testing (expectParsed, expectSuccessfulParse)
import Data.Text (Text)
import Data.Vector ((!))
import qualified Data.Vector as Vector
import Day4 (puzzleParser, solve1, solve2)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (parse)
import Text.RawString.QQ (r)

spec :: Spec
spec =
  let parsedSimplePuzzle = parse puzzleParser "" simplePuzzle
   in describe "Day4Spec" $ do
        it "parses at all" $ do
          expectSuccessfulParse parsedSimplePuzzle False
          expectParsed parsedSimplePuzzle $ \(RectangularGrid puzz) -> do
            Vector.length puzz `shouldBe` 10
            Vector.length (puzz ! 0) `shouldBe` 10
            Vector.length (Vector.last puzz) `shouldBe` 10
        it "solves part 1" $
          expectParsed parsedSimplePuzzle $ \puzz ->
            solve1 puzz `shouldBe` 13
        it "solves part 2" $
          expectParsed parsedSimplePuzzle $ \puzz ->
            solve2 puzz `shouldBe` 43
          

simplePuzzle :: Text
simplePuzzle =
  [r|..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.
|]
