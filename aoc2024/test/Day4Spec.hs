{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Day4Spec where

import AoC.Data.Grid.Rectangular (RectangularGrid (..))
import AoC.Parser.Testing (expectParsed, expectSuccessfulParse)
import Data.Text (Text)
import Data.Vector ((!))
import Day4 (XMas (..), puzzleParser, solve1, solve2)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (parse)
import Text.RawString.QQ (r)

gridSize :: RectangularGrid a -> Int
gridSize (RectangularGrid grid) =
  let nRows = length grid
      nCols = if nRows == 0 then 0 else length (grid ! 0)
   in nRows * nCols

spec :: Spec
spec =
  describe "Day4Spec" $
    let parsedExamplePuzzle = parse puzzleParser "" examplePuzzle
     in do
          describe "part 1" $ do
            it "parses" $ do
              expectSuccessfulParse parsedExamplePuzzle True
              expectParsed parsedExamplePuzzle $ \puzz -> gridSize puzz `shouldBe` 100
              expectParsed parsedExamplePuzzle $ \(RectangularGrid grid) -> do
                grid ! 0 ! 4 `shouldBe` X
                grid ! 3 ! 3 `shouldBe` M
            it "solves" $
              expectParsed parsedExamplePuzzle $
                \puzz -> solve1 puzz `shouldBe` 18
          describe "part 2" $
            it "sovles" $
              expectParsed parsedExamplePuzzle $
                \puzz -> solve2 puzz `shouldBe` 9

examplePuzzle :: Text
examplePuzzle =
  [r|MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX|]
