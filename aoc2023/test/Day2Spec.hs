{-# LANGUAGE OverloadedStrings #-}

module Day2Spec where

import AoC.Parser.Testing (expectParsed)
import Data.Monoid (Sum (..))
import qualified Data.Text as T
import Day2
  ( Game (..),
    Limits (..),
    PuzzleRound (..),
    blueCubeRound,
    day2Parser,
    greenCubeRound,
    redCubeRound,
    solve2,
    solver,
  )
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (parse)

spec :: Spec
spec = describe "day2" $ do
  describe "day 2 data" $ describe "PuzzleRound monoid" $ do
    it "combines rounds with cubes as expeced" $ do
      redCubeRound 3 <> redCubeRound 4 `shouldBe` redCubeRound 7
      redCubeRound 5 <> greenCubeRound 6 `shouldBe` PuzzleRound (Just . Sum $ 5) (Just . Sum $ 6) Nothing
  describe "day 2 parser" $
    it "parses the example puzzle" $
      expectParsed (parse day2Parser "" examplePuzzle) (`shouldBe` exampleGames)
  describe "day 2 solver" $ do
    it "gets the right part 1 example answer" $
      expectParsed
        (parse day2Parser "" examplePuzzle)
        ( \puzz ->
            solver (Limits 12 13 14) puzz `shouldBe` 8
        )
    it "gets the right part 2 example answer" $
      expectParsed
        (parse day2Parser "" examplePuzzle)
        ( \puzz ->
            solve2 puzz `shouldBe` 2286
        )

exampleGames :: [Game]
exampleGames =
  [ Game
      { gameId = 1,
        rounds = [blueCubeRound 3 <> redCubeRound 4, redCubeRound 1 <> greenCubeRound 2 <> blueCubeRound 6, greenCubeRound 2]
      },
    Game
      { gameId = 2,
        rounds = [blueCubeRound 1 <> greenCubeRound 2, greenCubeRound 3 <> blueCubeRound 4 <> redCubeRound 1, greenCubeRound 1 <> blueCubeRound 1]
      },
    Game
      { gameId = 3,
        rounds = [greenCubeRound 8 <> blueCubeRound 6 <> redCubeRound 20, blueCubeRound 5 <> redCubeRound 4 <> greenCubeRound 13, greenCubeRound 5 <> redCubeRound 1]
      },
    Game
      { gameId = 4,
        rounds = [greenCubeRound 1 <> redCubeRound 3 <> blueCubeRound 6, greenCubeRound 3 <> redCubeRound 6, greenCubeRound 3 <> blueCubeRound 15 <> redCubeRound 14]
      },
    Game
      { gameId = 5,
        rounds = [redCubeRound 6 <> blueCubeRound 1 <> greenCubeRound 3, blueCubeRound 2 <> redCubeRound 1 <> greenCubeRound 2]
      }
  ]

examplePuzzle :: T.Text
examplePuzzle =
  T.unlines
    [ "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
      "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
      "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
      "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
      "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
    ]
