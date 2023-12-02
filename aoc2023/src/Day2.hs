{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Day2 where

import AoC.Parser (Parser)
import qualified AoC.Parser as Parser
import Data.Foldable (foldl')
import Data.Functor (void)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum (..))
import Text.Megaparsec (sepBy, sepEndBy)
import Text.Megaparsec.Byte (eol)
import Text.Megaparsec.Byte.Lexer (decimal)

data MinSpanningCubes = MinSpanningCubes
  { minReds :: Sum Int,
    minGreens :: Sum Int,
    minBlues :: Sum Int
  }
  deriving (Eq, Show)

instance Semigroup MinSpanningCubes where
  (MinSpanningCubes {minReds = mr1, minBlues = mb1, minGreens = mg1})
    <> (MinSpanningCubes {minReds = mr2, minBlues = mb2, minGreens = mg2}) =
      MinSpanningCubes (mr1 `max` mr2) (mg1 `max` mg2) (mb1 `max` mb2)

instance Monoid MinSpanningCubes where
  mempty = MinSpanningCubes mempty mempty mempty

data Limits = Limits
  { maxReds :: Int,
    maxGreens :: Int,
    maxBlues :: Int
  }
  deriving (Eq, Show)

data PuzzleRound = PuzzleRound
  { redCubes :: Maybe (Sum Int),
    greenCubes :: Maybe (Sum Int),
    blueCubes :: Maybe (Sum Int)
  }
  deriving (Eq, Show)

instance Semigroup PuzzleRound where
  (PuzzleRound {redCubes = rc1, greenCubes = gc1, blueCubes = bc1})
    <> (PuzzleRound {redCubes = rc2, greenCubes = gc2, blueCubes = bc2}) = PuzzleRound {redCubes = rc1 <> rc2, greenCubes = gc1 <> gc2, blueCubes = bc1 <> bc2}

instance Monoid PuzzleRound where
  mempty = PuzzleRound mempty mempty mempty

redCubeRound :: Int -> PuzzleRound
redCubeRound n = PuzzleRound (Just . Sum $ n) mempty mempty

greenCubeRound :: Int -> PuzzleRound
greenCubeRound n = PuzzleRound mempty (Just . Sum $ n) mempty

blueCubeRound :: Int -> PuzzleRound
blueCubeRound n = PuzzleRound mempty mempty (Just . Sum $ n)

roundComponentConstructorParser :: Parser (Int -> PuzzleRound)
roundComponentConstructorParser =
  Parser.fromMap $
    Map.fromList
      [ ("blue", blueCubeRound),
        ("red", redCubeRound),
        ("green", greenCubeRound)
      ]

roundComponentParser :: Parser PuzzleRound
roundComponentParser =
  do
    cubes <- decimal <* " "
    constructor <- roundComponentConstructorParser
    pure $ constructor cubes

roundParser :: Parser PuzzleRound
roundParser = mconcat <$> roundComponentParser `sepBy` ", "

gameParser :: Parser Game
gameParser = do
  gid <- "Game " *> decimal
  void ": "
  gameRounds <- roundParser `sepBy` "; "
  pure $ Game gameRounds gid

data Game = Game {rounds :: [PuzzleRound], gameId :: Int} deriving (Eq, Show)

day2Parser :: Parser [Game]
day2Parser = gameParser `sepEndBy` eol

roundPossible :: Limits -> PuzzleRound -> Bool
roundPossible (Limits {maxReds, maxGreens, maxBlues}) (PuzzleRound {redCubes, greenCubes, blueCubes}) =
  let orTrue = fromMaybe True
      gte n = (Sum n >=)
   in orTrue (gte maxReds <$> redCubes) && orTrue (gte maxGreens <$> greenCubes) && orTrue (gte maxBlues <$> blueCubes)

gamePossible :: Limits -> Game -> Bool
gamePossible limits (Game {rounds}) =
  foldl' (\acc puzzleRound -> acc && roundPossible limits puzzleRound) True rounds

power :: MinSpanningCubes -> Int
power (MinSpanningCubes {minReds, minGreens, minBlues}) =
  getSum $ minReds * minGreens * minBlues

solver :: Limits -> [Game] -> Int
solver limits =
  foldl' (\acc game -> if gamePossible limits game then acc + gameId game else acc) 0

solve2 :: [Game] -> Int
solve2 games =
  let spanningCubesForRound (PuzzleRound {greenCubes, redCubes, blueCubes}) =
        MinSpanningCubes
          (fromMaybe mempty redCubes)
          (fromMaybe mempty greenCubes)
          (fromMaybe mempty blueCubes)
   in foldl'
        ( \acc (Game {rounds}) ->
            acc + power (foldMap spanningCubesForRound rounds)
        )
        0
        games
