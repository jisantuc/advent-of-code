{-# LANGUAGE OverloadedStrings #-}

module Day3 where

import AoC.Parser (Parser)
import Data.Coerce (coerce)
import Data.Functor (void, ($>))
import Data.Maybe (catMaybes, maybeToList)
import Data.Monoid (Sum (..))
import Text.Megaparsec (eof, lookAhead, many, sepBy, try, (<|>))
import Text.Megaparsec.Char (eol, printChar, string)
import Text.Megaparsec.Char.Lexer (decimal)

data Instruction = Multiply Integer Integer deriving (Eq, Show)

setMult :: Parser Bool
setMult = (string "don't()" $> False) <|> string "do()" $> True

instructionParser :: Parser (Maybe Instruction)
instructionParser = do
  c <- lookAhead printChar
  case c of
    'm' ->
      try
        ( do
            void "mul("
            x <- decimal
            void ","
            y <- decimal
            void ")"
            pure . Just $ Multiply x y
        )
        <|> (printChar $> Nothing)
    _ -> printChar $> Nothing

toggleableInstructionParser :: [Instruction] -> Bool -> Parser [Instruction]
toggleableInstructionParser accum enabled =
  (eof $> accum) <|> (eol *> toggleableInstructionParser accum enabled) <|> do
    c <- lookAhead printChar
    case c of
      'd' ->
        setMult >>= \state -> toggleableInstructionParser accum state
      'm' ->
        instructionParser >>= \res ->
          let acc = if enabled then accum <> maybeToList res else accum
           in toggleableInstructionParser acc enabled
      _ -> printChar *> toggleableInstructionParser accum enabled

puzzleParser :: Parser [Instruction]
puzzleParser = catMaybes . mconcat <$> (many instructionParser `sepBy` eol)

puzzleParser2 :: Parser [Instruction]
puzzleParser2 = toggleableInstructionParser [] True

solve1 :: [Instruction] -> Integer
solve1 = coerce . foldMap (\(Multiply x y) -> Sum (x * y))
