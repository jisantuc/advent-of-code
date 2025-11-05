{-# LANGUAGE OverloadedStrings #-}

module Day10 where

import AoC.Data.Grid.Rectangular (RectangularGrid, fromLists)
import AoC.Parser (Parser)
import Text.Megaparsec (many, sepBy, (<|>))
import Text.Megaparsec.Char (eol)
import Text.Megaparsec.Char.Lexer (decimal)

puzzleParser :: Parser (RectangularGrid Int)
puzzleParser =
  let rowParser = many ((-2 <$ ".") <|> decimal)
   in fromLists <$> sepBy rowParser eol
