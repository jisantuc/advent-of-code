{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Day1 (puzzleParser, puzzleParser2, solve2) where

import AoC.Parser (Parser)
import Data.Foldable (foldl')
import Text.Megaparsec (eof, sepEndBy, (<|>))
import Text.Megaparsec.Char (eol)
import Text.Megaparsec.Char.Lexer (decimal)

puzzleParser :: (Int, Int) -> Parser (Int, Int)
puzzleParser state@(initialValue, numZeros) =
  (state <$ eof)
    <|> ( (((negate <$> ("L" *> decimal)) <|> ("R" *> decimal)) <* eol)
            >>= puzzleParser
              . ( \case
                    n ->
                      ( mod (initialValue + n) 100,
                        if mod (initialValue + n) 100 == 0 then numZeros + 1 else numZeros
                      )
                )
        )

data Instruction = L Int | R Int deriving (Eq, Show)

puzzleParser2 :: Parser [Instruction]
puzzleParser2 =
  ( L
      <$> ("L" *> decimal)
        <|> (R <$> ("R" *> decimal))
  )
    `sepEndBy` eol

solve2 :: Int -> [Instruction] -> Int
solve2 initialValue =
  let goNext (v, initialCrosses) spinDistance calcNextVal calcNextValCrossed =
        let (hundreds, leftOver) = divMod spinDistance 100
            nextVal = calcNextVal v leftOver
            nextValCrossed = calcNextValCrossed v nextVal
         in (nextVal `mod` 100, initialCrosses + hundreds + if nextValCrossed then 1 else 0)
   in snd
        . foldl'
          ( \acc instr -> case instr of
              L n -> goNext acc n (-) (\v n' -> n' <= 0 && v /= 0)
              R n -> goNext acc n (+) (\_ n' -> n' >= 100)
          )
          (initialValue, 0)
