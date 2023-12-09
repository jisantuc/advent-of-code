{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Day7Spec where

import AoC.Parser.Testing (expectParsed, expectSuccessfulParse)
import qualified Data.Text as T
import Day7
  ( Hand (..),
    HandType (..),
    Puzzle (..),
    classifyHand,
    classifyHandWithJoker,
    parser,
    solver1,
    solver2,
  )
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (parse)
import Text.RawString.QQ (r)

spec :: Spec
spec =
  let parsedExamplePuzzle = parse parser "" examplePuzzle
   in describe "day 7" $ do
        describe "parser" $ do
          it "parses the example puzzle at all" $ do
            expectSuccessfulParse parsedExamplePuzzle False
            expectParsed
              parsedExamplePuzzle
              ( \(Puzzle hands) ->
                  hands
                    `shouldBe` [ Hand [3, 2, 10, 3, 13] 765,
                                 Hand [10, 5, 5, 11, 5] 684,
                                 Hand [13, 13, 6, 7, 7] 28,
                                 Hand [13, 10, 11, 11, 10] 220,
                                 Hand [12, 12, 12, 11, 14] 483
                               ]
              )
        describe "hand classification" $
          do
            it "classifies the example hands correctly" $ do
              expectParsed
                parsedExamplePuzzle
                ( \(Puzzle hands) ->
                    classifyHand <$> hands
                      `shouldBe` [ OnePair,
                                   ThreeOfAKind,
                                   TwoPairs,
                                   TwoPairs,
                                   ThreeOfAKind
                                 ]
                )
              classifyHand (Hand [1, 1, 1, 1, 2] 1) `shouldBe` FourOfAKind
              classifyHand (Hand [1, 2, 1, 2, 1] 1) `shouldBe` FullHouse
              classifyHand (Hand [1, 1, 1, 1, 1] 1) `shouldBe` FiveOfAKind
              classifyHand (Hand [1, 2, 3, 4, 5] 1) `shouldBe` HighCard
            it "classifies hands with jokers correctly" $ do
              classifyHandWithJoker (Hand [3, 2, 10, 3, 13] 1) `shouldBe` OnePair
              classifyHandWithJoker (Hand [10, 5, 5, 11, 5] 1) `shouldBe` FourOfAKind
              classifyHandWithJoker (Hand [13, 10, 11, 11, 10] 1) `shouldBe` FourOfAKind
              classifyHandWithJoker (Hand [12, 12, 12, 11, 14] 1) `shouldBe` FourOfAKind
            it "ranks hands correctly" $ do
              Hand [3, 2, 10, 3, 13] 10 > Hand [3, 1, 10, 3, 13] 10
                `shouldBe` True
              Hand [3, 2, 10, 3, 3] 10 > Hand [3, 1, 10, 3, 13] 10
                `shouldBe` True
              Hand [1, 4, 4, 4, 4] 10 > Hand [2, 2, 2, 2, 1] 10
                `shouldBe` False
              Hand [7, 7, 8, 8, 8] 1 > Hand [7, 7, 7, 8, 8] 1
                `shouldBe` True
              Hand [7, 7, 8, 8, 8] 1 >= Hand [7, 7, 7, 8, 8] 1
                `shouldBe` True
              Hand [11, 7, 7, 7, 1] 1 < Hand [1, 7, 7, 7, 7] 1
                `shouldBe` True
        describe "part 1 solver" $ do
          it "gets the right answer for the sample puzzle" $
            expectParsed
              parsedExamplePuzzle
              ( \puzz ->
                  solver1 puzz `shouldBe` 6440
              )
        describe "part 2 solver" $ do
          it "gets the right answer for the example puzzle part 2" $
            expectParsed
              parsedExamplePuzzle
              (\puzz -> solver2 puzz `shouldBe` 5905)

examplePuzzle :: T.Text
examplePuzzle =
  [r|32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483|]
