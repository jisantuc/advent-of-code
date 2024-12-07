{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Day6Spec where

import AoC.Data.Grid.Rectangular (Direction (..), RectangularGrid (..))
import AoC.Parser.Testing (expectParsed, expectSuccessfulParse)
import Data.Text (Text)
import Data.Vector ((!))
import qualified Data.Vector as Vector
import Day6 (FloorSpace (..), puzzleParser)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (parse)
import Text.RawString.QQ (r)

spec :: Spec
spec = describe "Day6Spec" $ do
  describe "part 1" $ do
    it "parses" $
      let parsedExamplePuzzle = parse puzzleParser "" examplePuzzle
       in do
            expectSuccessfulParse (parse puzzleParser "" examplePuzzle) True
            expectParsed parsedExamplePuzzle $ \(RectangularGrid g) -> do
              Vector.length g `shouldBe` 10
              g ! 0 ! 0 `shouldBe` ((0, 0), Open)
              g ! 0 ! 4 `shouldBe` ((0, 4), Obstacle)
              g ! 6 ! 4 `shouldBe` ((6, 4), Guard (Up 1))

examplePuzzle :: Text
examplePuzzle =
  [r|....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...|]
