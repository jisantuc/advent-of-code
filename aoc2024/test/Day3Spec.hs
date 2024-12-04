{-# LANGUAGE OverloadedStrings #-}

module Day3Spec where

import AoC.Parser.Testing (expectParsed, expectSuccessfulParse)
import Day3 (Instruction (..), instructionParser, puzzleParser, puzzleParser2, solve1)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (parse)

spec :: Spec
spec = describe "Day3Spec" $ do
  describe "parsing" $ do
    it "parses simple" $
      let parsedSimple = parse instructionParser "" "mul(123,456)"
       in do
            expectSuccessfulParse parsedSimple True
            expectParsed parsedSimple (\x -> x `shouldBe` Just (Multiply 123 456))
    it "parses complex" $
      let parsedComplex = parse puzzleParser "" "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))\n"
          parsedSemiComplex = parse puzzleParser "" "xmul(2,4)mul(3,5)mul[3,7]\n"
       in do
            expectSuccessfulParse parsedSemiComplex True
            expectParsed parsedSemiComplex (\x -> x `shouldBe` [Multiply 2 4, Multiply 3 5])
            expectSuccessfulParse parsedComplex True
            expectParsed parsedComplex (\x -> x `shouldBe` [Multiply 2 4, Multiply 5 5, Multiply 11 8, Multiply 8 5])
    it "parses part 2 rules" $
      let parsedComplex = parse puzzleParser2 "" "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
       in do
            expectSuccessfulParse parsedComplex True
            expectParsed parsedComplex (\x -> x `shouldBe` [Multiply 2 4, Multiply 8 5])

    it "sovles part 1" $
      solve1 [Multiply 2 4, Multiply 5 5, Multiply 11 8, Multiply 8 5] `shouldBe` 161
