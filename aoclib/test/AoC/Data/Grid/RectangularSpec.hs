module AoC.Data.Grid.RectangularSpec where

import AoC.Data.Grid.Rectangular (manhattanDistance)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)

spec :: Spec
spec =
  describe "Rectangular grid" $
    describe "Manhattan distance" $ do
      it "calculates good distances for some example points" $ do
        manhattanDistance (0, 1) (-12, 28) `shouldBe` 39
        manhattanDistance (3, -4) (22, 5) `shouldBe` 19 + 9
      prop "calculates good distances from origin" $
        \squareParam flipY ->
          manhattanDistance (0, 0) (squareParam, if flipY then negate squareParam else squareParam)
            `shouldBe` 2 * abs squareParam
      prop "calculates the same distance for rotated points" $
        \p1@(x1, y1) p2@(x2, y2) ->
          manhattanDistance p1 p2 `shouldBe` manhattanDistance (y1, x1) (y2, x2)
