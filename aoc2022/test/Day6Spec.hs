module Day6Spec where

import Day6 (solvePart1, solvePart2)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "day 6" $ do
  it "solves part 1 example" $ do
    solvePart1 examplePuzzle1 `shouldBe` Just 7
    solvePart1 examplePuzzle2 `shouldBe` Just 5
    solvePart1 examplePuzzle3 `shouldBe` Just 6
    solvePart1 examplePuzzle4 `shouldBe` Just 10
    solvePart1 examplePuzzle5 `shouldBe` Just 11
  it "solve part 2 example" $ do
    solvePart2 examplePuzzle1 `shouldBe` Just 19
    solvePart2 examplePuzzle2 `shouldBe` Just 23
    solvePart2 examplePuzzle3 `shouldBe` Just 23
    solvePart2 examplePuzzle4 `shouldBe` Just 29
    solvePart2 examplePuzzle5 `shouldBe` Just 26

examplePuzzle1 :: String
examplePuzzle1 = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"

examplePuzzle2 :: String
examplePuzzle2 = "bvwbjplbgvbhsrlpgdmjqwftvncz"

examplePuzzle3 :: String
examplePuzzle3 = "nppdvjthqldpwncqszvftbrmjlhg"

examplePuzzle4 :: String
examplePuzzle4 = "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"

examplePuzzle5 :: String
examplePuzzle5 = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
