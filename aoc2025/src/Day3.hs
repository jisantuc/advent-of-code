{-# OPTIONS_GHC -Wno-type-defaults #-}

module Day3 (puzzleParser, solve1) where

import AoC.Parser (Parser)
import qualified AoC.Parser as Parser
import Data.Foldable (Foldable (foldMap'), foldl')
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

maxJoltage :: [Int] -> Int
maxJoltage bank =
  (\(x, y) -> read $ show x <> show y)
    . foldl'
      ( \acc@(r1, r2) (j, next) ->
          case next of
            -- when there's no next value, only the right digit can change
            Nothing -> if j > r2 then (r1, j) else acc
            -- when there is a next value, it's guaranteed >= 0, so
            -- (j, 0) is a safe replacement if j is bigger than the lead digit,
            -- and if j is bigger than the second digit, (r1, j) is correct
            Just _ -> if j > r1 then (j, 0) else if j > r2 then (r1, j) else acc
      )
      (0, 0)
    $ zip bank ((Just <$> drop 1 bank) <> [Nothing])

solve1 :: [[Int]] -> Int
solve1 = getSum . foldMap' (Sum . maxJoltage)
