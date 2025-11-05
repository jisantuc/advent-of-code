{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Day10Spec where

import AoC.Data.Grid.Rectangular (RectangularGrid (RectangularGrid), debugShow)
import AoC.Parser.Testing (expectParsed, expectSuccessfulParse)
import Data.Text (Text)
import Data.Vector ((!))
import Day10 (puzzleParser)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (parse)
import Text.RawString.QQ (r)
import Debug.Trace (trace)
import Data.Char (intToDigit)

spec :: Spec
spec =
  describe "Day10Spec" $
    let parsedSimplePuzzle = parse puzzleParser "" simpleTopo
     in do
          describe "parsing" $ do
            it "parses the simple puzzle" $ do
              expectSuccessfulParse parsedSimplePuzzle False
              expectParsed parsedSimplePuzzle $ \p@(RectangularGrid puzz) -> trace (debugShow intToDigit p) $ do
                puzz ! 0 ! 3 `shouldBe` 0
                puzz ! 1 ! 3 `shouldBe` 1
                puzz ! 3 ! 4 `shouldBe` 4
                puzz ! 6 ! 6 `shouldBe` 9
                puzz ! 6 ! 0 `shouldBe` 9

simpleTopo :: Text
simpleTopo =
  [r|...0...
...1...
...2...
6543456
7.....7
8.....8
9.....9
|]
