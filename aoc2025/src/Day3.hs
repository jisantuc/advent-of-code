{-# OPTIONS_GHC -Wno-type-defaults #-}

module Day3 (puzzleParser, solve1) where

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
                -- when there's _more than batteriesToUse digits remaining_ (including j),
                -- the digit can go in the first slot that it's greater than,
                -- and all remaining digits are zeroed.
                then case findIndex (j >) acc of
                  Just idx -> take idx acc <> [j] <> replicate (batteriesToUse - idx - 1) 0
                  Nothing -> acc
                -- when there's _less than or equal to batteriesToUse digits remaining_ (including j),
                -- I think I have to treat the head of acc as fixed, and find the place to insert the
                -- value in the tail, but I'm sleepy and I can't work that out right now
                else undefined
      )
      (replicate batteriesToUse 0)
    $ zip bank (drop 1 $ tails bank)

solve1 :: [[Int]] -> Int
solve1 = getSum . foldMap' (Sum . maxJoltage 2)
