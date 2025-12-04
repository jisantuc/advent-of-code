{-# OPTIONS_GHC -Wno-type-defaults #-}

module Day3 (puzzleParser, solve1, solve2) where

import AoC.Parser (Parser)
import qualified AoC.Parser as Parser
import Data.Foldable (Foldable (foldMap'), foldl')
import Data.List (findIndex, tails)
import qualified Data.Map.Strict as Map
import Data.Monoid (Sum (..))
import qualified Data.Text as Text
import Text.Megaparsec (many, sepEndBy)
import Text.Megaparsec.Char (eol)

puzzleParser :: Parser [[Int]]
puzzleParser =
  let digits =
        Parser.fromMap
          (Map.fromList (zip (Text.pack . show <$> [0 .. 9]) [0 .. 9]))
   in many digits `sepEndBy` eol

maxJoltage :: Int -> [Int] -> Int
maxJoltage batteriesToUse bank =
  (read . foldMap' show)
    . foldl'
      ( \acc (j, next) ->
            case next of
              xs ->
                if length xs + 1 >= batteriesToUse
                  then case findIndex (j >) acc of
                    Just idx -> take idx acc <> [j] <> replicate (batteriesToUse - idx - 1) 0
                    Nothing -> acc
                  else
                    let canFill = length next + 1
                        fixedDigits = batteriesToUse - canFill
                        (fixed, rest) = splitAt fixedDigits acc
                     in case findIndex (j >) rest of
                          Just idx -> fixed <> take idx rest <> [j] <> replicate (length rest - idx - 1) 0
                          Nothing -> acc
      )
      (replicate batteriesToUse 0)
    $ zip bank (drop 1 $ tails bank)

solve1 :: [[Int]] -> Int
solve1 = getSum . foldMap' (Sum . maxJoltage 2)

solve2 :: [[Int]] -> Int
solve2 = getSum . foldMap' (Sum . maxJoltage 12)
