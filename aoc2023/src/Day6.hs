{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Day6 where

import AoC.Optimization (hillClimbWithStep)
import AoC.Parser (Parser)
import Data.Functor (void)
import Text.Megaparsec (between, eof, sepBy)
import Text.Megaparsec.Char (eol, hspace)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Monoid (Product(..))

data Race = Race {timeMilliseconds :: Int, recordDistance :: Int} deriving (Eq, Show)

newtype Puzzle = Puzzle [Race] deriving (Eq, Show)

parser :: Parser Puzzle
parser = do
  void "Time:"
  raceTimes <- between hspace eol (decimal `sepBy` hspace)
  void "Distance:"
  distances <- between hspace eof (decimal `sepBy` hspace)
  pure . Puzzle $ zipWith Race raceTimes distances

winningMovesForRace :: Race -> Int
winningMovesForRace (Race {timeMilliseconds, recordDistance}) =
  let f x = x * (timeMilliseconds - x)
      step = (timeMilliseconds `div` 20) `max` 1
      maxValue = fst $ hillClimbWithStep f 0 step
      winnersRightOfMax = takeWhile ((> recordDistance) . f) [maxValue ..]
      winnersLeftOfMax = takeWhile ((> recordDistance) . f) [maxValue, maxValue - 1 .. 0]
   in length (winnersRightOfMax ++ winnersLeftOfMax) - 1

solver1 :: Puzzle -> Int
solver1 (Puzzle races) =
  getProduct . mconcat $ Product . winningMovesForRace <$> races
