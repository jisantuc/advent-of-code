{-# LANGUAGE OverloadedStrings #-}

module Day1Spec where

import AoC.Parser.Testing (expectParsed)
import Day1 (day1Parser)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (runParser)

spec :: Spec
spec =
  let examplePuzzle = "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen"
   in do
        describe "day 1 parser" $
          it "parses the example" $
            expectParsed (runParser day1Parser "day 1 parse" examplePuzzle) (`shouldBe` [29, 83, 13, 24, 42, 14, 76])
