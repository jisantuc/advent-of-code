{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Day6 where

import Data.Foldable (foldl')
import qualified Data.Set as Set

newtype Chars = Chars [Char] deriving (Eq, Show)

snoc :: Int -> Chars -> Char -> Chars
snoc maxRun (Chars xs) x =
  let base = if length xs >= maxRun then drop 1 xs else xs
   in Chars $ base ++ [x]

emptyChars :: Chars
emptyChars = Chars []

data Needle = Needle
  { chars :: Chars,
    count :: Int
  }
  deriving (Eq, Show)

needleFound :: Int -> Chars -> Bool
needleFound run (Chars chars) = (length . Set.fromList $ chars) == run

solve :: Int -> [Char] -> Maybe Int
solve run xs =
  let needle =
        foldl'
          ( \n@(Needle {chars, count}) c ->
              if needleFound run chars then n else n {chars = snoc run chars c, count = count + 1}
          )
          (Needle emptyChars 0)
          xs
   in ( \case
          Needle {chars, count}
            | needleFound run chars ->
                Just count
          _ -> Nothing
      )
        needle

solvePart1 :: [Char] -> Maybe Int
solvePart1 = solve 4

solvePart2 :: [Char] -> Maybe Int
solvePart2 = solve 14
