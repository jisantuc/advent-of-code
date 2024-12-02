module Day2Spec where

import Day2
  ( PuzzleRow (..),
    PuzzleSafety (..),
    checkRow,
    checkRowWithDampener,
  )
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Day2Spec" $ do
  describe "part 1" $ do
    it "classifies example rows correctly" $ do
      checkRow (PuzzleRow [7, 6, 4, 2, 1]) `shouldBe` Safe
      checkRow (PuzzleRow [1, 2, 7, 8, 9]) `shouldBe` Unsafe
      checkRow (PuzzleRow [9, 7, 6, 2, 1]) `shouldBe` Unsafe
      checkRow (PuzzleRow [1, 3, 2, 4, 5]) `shouldBe` Unsafe
      checkRow (PuzzleRow [8, 6, 4, 4, 1]) `shouldBe` Unsafe
      checkRow (PuzzleRow [1, 3, 6, 7, 9]) `shouldBe` Safe
  describe "part 2" $ do
    it "classifies example rows correctly" $ do
      checkRowWithDampener (PuzzleRow [7, 6, 4, 2, 1]) `shouldBe` Safe
      checkRowWithDampener (PuzzleRow [1, 2, 7, 8, 9]) `shouldBe` Unsafe
      checkRowWithDampener (PuzzleRow [9, 7, 6, 2, 1]) `shouldBe` Unsafe
      checkRowWithDampener (PuzzleRow [1, 3, 2, 4, 5]) `shouldBe` Safe
      checkRowWithDampener (PuzzleRow [8, 6, 4, 4, 1]) `shouldBe` Safe
      checkRowWithDampener (PuzzleRow [1, 3, 6, 7, 9]) `shouldBe` Safe
      checkRowWithDampener (PuzzleRow [74, 76, 78, 79, 76]) `shouldBe` Safe
      checkRowWithDampener (PuzzleRow [38, 40, 43, 44, 44]) `shouldBe` Safe
