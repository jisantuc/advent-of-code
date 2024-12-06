{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Day5Spec where

import AoC.Parser.Testing (expectParsed, expectSuccessfulParse)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Day5
  ( PrecedenceRules (..),
    Puzzle (..),
    fixInvalidLine,
    precedenceParser,
    puzzleParser,
    solve1,
    solve2,
    updateIsValid,
  )
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (parse)
import Text.RawString.QQ (r)

spec :: Spec
spec = describe "Day5Spec" $ do
  describe "part 1" $
    do
      describe "parses" $ do
        it "precedence" $
          let parsedExamplePrecedence = parse precedenceParser "" examplePrecedenceRules
           in do
                expectSuccessfulParse parsedExamplePrecedence True
                expectParsed parsedExamplePrecedence $ \precedence -> do
                  Map.lookup 47 (unPrecedenceRules precedence) `shouldBe` Just (Set.fromList [53, 13, 61, 29])
                  Map.lookup 53 (unPrecedenceRules precedence) `shouldBe` Just (Set.fromList [29, 13])
        it "whole puzzles" $
          let parsedExamplePuzzle = parse puzzleParser "" examplePuzzle
           in do
                expectSuccessfulParse parsedExamplePuzzle True
                expectParsed parsedExamplePuzzle $ \puzz -> do
                  (length . updates) puzz `shouldBe` 6
                  (last . updates) puzz `shouldBe` [97, 13, 75, 29, 47]
      it "solves the example" $
        let parsedExamplePuzzle = parse puzzleParser "" examplePuzzle
         in expectParsed parsedExamplePuzzle $ \puzz -> solve1 puzz `shouldBe` 143
  describe "part 2" $
    let parsedExamplePuzzle = parse puzzleParser "" examplePuzzle
     in do
          it "identifies invalid lines" $
            expectParsed parsedExamplePuzzle $ \(Puzzle {rules, updates}) ->
              length (filter (not . updateIsValid rules) updates) `shouldBe` 3
          it "fixes invalid lines" $ do
            expectParsed parsedExamplePuzzle $ \(Puzzle {rules}) -> do
              fixInvalidLine rules [75, 97, 47, 61, 53] `shouldBe` [97, 75, 47, 61, 53]
              fixInvalidLine rules [61, 13, 29] `shouldBe` [61, 29, 13]
              fixInvalidLine rules [97, 13, 75, 29, 47] `shouldBe` [97, 75, 47, 29, 13]
          it "solves" $
            expectParsed parsedExamplePuzzle $
              \puzz -> solve2 puzz `shouldBe` 123

examplePrecedenceRules :: Text
examplePrecedenceRules =
  [r|47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13
|]

examplePuzzle :: Text
examplePuzzle =
  [r|47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47|]
