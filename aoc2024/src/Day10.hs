{-# LANGUAGE OverloadedStrings #-}

module Day10 where

import AoC.Data.Grid.Rectangular (RectangularGrid, fromLists)
import AoC.Parser (Parser)
import Text.Megaparsec (choice, many, sepBy, (<|>))
import Text.Megaparsec.Char (eol)
import Text.Megaparsec.Char.Lexer (decimal)

puzzleParser :: Parser (RectangularGrid Int)
puzzleParser =
  let rowParser =
        many $
          choice
            [ 0 <$ "0",
              1 <$ "1",
              2 <$ "2",
              3 <$ "3",
              4 <$ "4",
              5 <$ "5",
              6 <$ "6",
              7 <$ "7",
              8 <$ "8",
              9 <$ "9",
              0 <$ "."
            ]
   in fromLists <$> sepBy rowParser eol
