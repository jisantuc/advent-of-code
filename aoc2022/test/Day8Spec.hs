module Day8Spec where

import qualified Data.Set as Set
import qualified Data.Vector as Vector
import Day8
  ( FromSide (..),
    FromTop (..),
    Grid (..),
    solvePart1,
    transpose,
    visibleTreesFromSide,
    visibleTreesFromTopBottom,
  )
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "day 8" $ do
    describe "grid ops" $ do
      it "tranpsoses matrices" $ do
        transpose smallGrid `shouldBe` transposedSmallGrid
        transpose transposedSmallGrid `shouldBe` smallGrid
        transpose (transpose smallGrid) `shouldBe` smallGrid
        transpose (transpose transposedSmallGrid) `shouldBe` transposedSmallGrid
      it "finds the right visible trees from the left and right" $ do
        visibleTreesFromSide LeftSide exampleGrid 0
          `shouldBe` Set.fromList [(0, 0), (0, 3)]
        visibleTreesFromSide LeftSide exampleGrid 1
          `shouldBe` Set.fromList [(1, 0), (1, 1)]
        visibleTreesFromSide LeftSide exampleGrid 3
          `shouldBe` Set.fromList [(3, 0), (3, 2), (3, 4)]
        visibleTreesFromSide RightSide exampleGrid 2
          `shouldBe` Set.fromList [(2, 4), (2, 3), (2, 1), (2, 0)]
        visibleTreesFromSide RightSide exampleGrid 4
          `shouldBe` Set.fromList [(4, 4), (4, 3)]
      it "finds the right visible trees from the top and bottom" $ do
        visibleTreesFromTopBottom Top exampleGrid 0
          `shouldBe` Set.fromList [(0, 0), (2, 0)]
        visibleTreesFromTopBottom Top exampleGrid 2
          `shouldBe` Set.fromList [(0, 2), (1, 2)]
        visibleTreesFromTopBottom Bottom exampleGrid 1
          `shouldBe` Set.fromList [(4, 1)]
        visibleTreesFromTopBottom Bottom exampleGrid 3
          `shouldBe` Set.fromList [(4, 3)]
        visibleTreesFromTopBottom Bottom exampleGrid 4
          `shouldBe` Set.fromList [(4, 4), (3, 4)]
      it "finds the right answer for the example puzzle" $ do
        solvePart1 exampleGrid `shouldBe` 21

smallGrid :: Grid
smallGrid =
  Grid $
    Vector.fromList [Vector.fromList [1, 2], Vector.fromList [3, 4]]

transposedSmallGrid :: Grid
transposedSmallGrid =
  Grid $
    Vector.fromList [Vector.fromList [1, 3], Vector.fromList [2, 4]]

exampleGrid :: Grid
exampleGrid =
  Grid $
    Vector.fromList
      [ Vector.fromList [3, 0, 3, 7, 3],
        Vector.fromList [2, 5, 5, 1, 2],
        Vector.fromList [6, 5, 3, 3, 2],
        Vector.fromList [3, 3, 5, 4, 9],
        Vector.fromList [3, 5, 3, 9, 0]
      ]
