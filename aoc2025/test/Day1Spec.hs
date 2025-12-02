{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Day1Spec where

import AoC.Parser.Testing (expectParsed)
import Data.Text (Text)
import qualified Data.Text as Text
import Day1 (puzzleParser, puzzleParser2, solve2)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (parse)
import Text.RawString.QQ (r)

spec :: Spec
spec =
  let parsedExample1 = parse (puzzleParser (50, 0)) "" simplePuzzle
      parsedExample2 = parse puzzleParser2 "" simplePuzzle
      parsedSuperSimple = parse puzzleParser2 "" simplerPuzzle
   in describe "Day1Spec" $ do
        it "solves part 1" $ do
          expectParsed parsedExample1 $ \puzz ->
            snd puzz `shouldBe` 3
        describe "solves part 2" $ do
          it "solves the super simple puzzle" $
            expectParsed parsedSuperSimple $ \puzz ->
              solve2 99 puzz `shouldBe` 2
          it "solves the actual example" $
            expectParsed parsedExample2 $ \puzz ->
              solve2 50 puzz `shouldBe` 6

simplePuzzle :: Text
simplePuzzle =
  Text.unlines
    [ "L68", -- (82, 1)
      "L30", -- (52, 1)
      "R48", -- (0, 2)
      "L5", -- (95, 2)
      "R60", -- (55, 3)
      "L55", -- (0, 4)
      "L1", -- (99, 4)
      "L99", -- (0, 5)
      "R14", -- (14, 5)
      "L82" -- (68, 6)
    ]

simplerPuzzle :: Text
simplerPuzzle =
  [r|R2
L2
|]
