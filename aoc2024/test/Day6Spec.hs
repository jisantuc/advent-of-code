{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Day6Spec where

import AoC.Data.Grid.Rectangular (Direction (..), RectangularGrid (..))
import qualified AoC.Data.Grid.Rectangular as RectangularGrid
import AoC.Parser.Testing (expectParsed, expectSuccessfulParse)
import Data.Text (Text)
import Data.Vector ((!))
import qualified Data.Vector as Vector
import Day6
  ( FloorSpace (..),
    checkForLoop,
    isLoop,
    mapGuardPath,
    puzzleParser,
    solve1,
    solve2,
  )
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (parse)
import Text.RawString.QQ (r)

spec :: Spec
spec = describe "Day6Spec" $ do
  describe "part 1" $ do
    it "parses" $
      let parsedExamplePuzzle = parse puzzleParser "" examplePuzzle
       in do
            expectSuccessfulParse (parse puzzleParser "" examplePuzzle) False
            expectParsed parsedExamplePuzzle $ \(RectangularGrid g) -> do
              Vector.length g `shouldBe` 10
              g ! 0 ! 0 `shouldBe` ((0, 0), Open)
              g ! 0 ! 4 `shouldBe` ((0, 4), Obstacle)
              g ! 6 ! 4 `shouldBe` ((6, 4), Guard (Up ()))
    it "solves" $
      let parsedExamplePuzzle = parse puzzleParser "" examplePuzzle
       in do
            expectParsed parsedExamplePuzzle $ \puzz ->
              solve1 puzz `shouldBe` 41
  describe "part 2" $ do
    it "finds some loops" $
      let parsedSimpleLoop = parse puzzleParser "" simpleLoopPuzzle
          parsedBiggerLoop = parse puzzleParser "" biggerLoopPuzzle
       in do
            expectParsed parsedSimpleLoop $ \puzz -> isLoop (mapGuardPath puzz) `shouldBe` True
            expectParsed parsedBiggerLoop $ \puzz -> isLoop (mapGuardPath puzz) `shouldBe` True
    it "solves" $
      let parsedExamplePuzzle = parse puzzleParser "" examplePuzzle
       in expectParsed parsedExamplePuzzle $ \puzz -> do
            checkForLoop puzz (mapGuardPath puzz) (6, 4) (5, 8) `shouldBe` Nothing
            checkForLoop puzz (mapGuardPath puzz) (6, 4) (0, 0) `shouldBe` Nothing
            checkForLoop puzz (mapGuardPath puzz) (6, 4) (1, 5) `shouldBe` Nothing
            solve2 puzz `shouldBe` 6

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

simpleLoopPuzzle :: Text
simpleLoopPuzzle =
  [r|..#..
..^.#
.#...
...#|]

biggerLoopPuzzle :: Text
biggerLoopPuzzle =
  [r|....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#..#......
......#...|]
