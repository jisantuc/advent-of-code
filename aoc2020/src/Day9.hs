module Day9 where

import qualified Data.Vector as V
import Day1 (findTarget)
import Parser (Parser)
import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char (eol)
import Text.Megaparsec.Char.Lexer (decimal)

solve :: Int -> Int -> V.Vector Int -> Int
solve startIndex lookback nums =
  let x = V.unsafeIndex nums startIndex
      preamble = V.take lookback . V.drop (startIndex - lookback) $ nums
   in case findTarget (toInteger x) (toInteger <$> V.toList preamble) of
        Just _ -> solve (startIndex + 1) lookback nums
        Nothing -> x

contiguousRangeN :: Int -> V.Vector Int -> V.Vector [Int]
contiguousRangeN size nums =
  go size $ (: []) <$> nums
  where
    go 0 acc = acc
    go 1 acc = acc
    go n acc =
      let rotatedForN = V.fromList $ V.toList (V.drop (n - 1) nums) ++ V.toList (V.take (n - 1) nums)
       in go (n - 1) $ V.zipWith (:) rotatedForN acc

findTargetRange :: Int -> V.Vector [Int] -> Maybe [Int]
findTargetRange target ranges =
  V.find (\x -> sum x == target) ranges

solvePart2 :: Int -> V.Vector Int -> (Int, Int)
solvePart2 target nums =
  go 2 nums
  where
    go rangeSize inp =
      case findTargetRange target (contiguousRangeN rangeSize inp) of
        Just range ->
          (minimum range, maximum range)
        Nothing ->
          go (rangeSize + 1) inp

puzzleParser :: Parser (V.Vector Int)
puzzleParser = V.fromList <$> (sepBy decimal eol)
