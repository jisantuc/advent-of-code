{-# LANGUAGE OverloadedStrings #-}

module Day2 where

import AoC.Parser (Parser)
import Data.Foldable (Foldable (foldMap'))
import qualified Data.Set as Set
import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char (eol)
import Text.Megaparsec.Char.Lexer (decimal)

data PuzzleSafety = Safe | Unsafe deriving (Eq, Show)

instance Semigroup PuzzleSafety where
  _ <> Safe = Safe
  Safe <> _ = Safe
  Unsafe <> Unsafe = Unsafe

instance Monoid PuzzleSafety where
  mempty = Unsafe

newtype PuzzleRow = PuzzleRow [Int] deriving (Eq, Show)

data DampenerState = DampenerState
  { accum :: PuzzleRow,
    spentErrorSuppression :: Bool,
    safety :: PuzzleSafety
  }
  deriving (Eq, Show)

checkRow :: PuzzleRow -> PuzzleSafety
checkRow (PuzzleRow nums) =
  let comparisons = zipWith compare nums (drop 1 nums)
      allowedDiffs = Set.fromList [1, 2, 3]
      diffs = zipWith (\x y -> abs (x - y)) nums (drop 1 nums)
      uniqueDiffs = Set.fromList diffs
      allIncreasing = all (\r -> r == EQ || r == GT) comparisons
      allDecreasing = all (\r -> r == EQ || r == LT) comparisons
   in if (allIncreasing || allDecreasing) && uniqueDiffs `Set.intersection` allowedDiffs == uniqueDiffs
        then Safe
        else Unsafe

excluding :: [a] -> Int -> [a]
excluding xs idx =
  let (h, t) = splitAt idx xs
   in take (idx - 1) h ++ t

checkRowWithDampener :: PuzzleRow -> PuzzleSafety
checkRowWithDampener (PuzzleRow []) = Unsafe
checkRowWithDampener row@(PuzzleRow nums) =
  if checkRow row == Safe
    then Safe
    else
      let allVariations = PuzzleRow . (nums `excluding`) <$> [0 .. length nums]
       in foldMap' checkRow allVariations

parser :: Parser [PuzzleRow]
parser = (PuzzleRow <$> decimal `sepBy` " ") `sepBy` eol

solve1 :: [PuzzleRow] -> IO ()
solve1 = print . length . filter (\row -> checkRow row == Safe)

solve2 :: [PuzzleRow] -> IO ()
solve2 = print . length . filter (\row -> checkRowWithDampener row == Safe)
