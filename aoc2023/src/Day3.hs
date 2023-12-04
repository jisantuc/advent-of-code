{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day3 where

import AoC.Data.Grid.Rectangular (Point)
import AoC.Parser (Parser)
import Data.Foldable (foldMap', foldl')
import Data.Functor (void)
import qualified Data.Map as Map
import Data.Monoid (Product (..), Sum (..))
import Text.Megaparsec (SourcePos (..), getSourcePos, many, sepBy, unPos, (<|>))
import Text.Megaparsec.Char (asciiChar, eol)
import Text.Megaparsec.Char.Lexer (decimal)

data EngineSchematicSymbol
  = -- start and end positions of the number
    Number Int Point Point
  | -- location of the non-number, non-dot symbol
    Symbol Char Point
  | Dot
  deriving (Eq, Show)

width :: Point -> Point -> Int
width (_, col1) (_, col2) = abs (col1 - col2) + 1

newtype Puzzle = Puzzle [EngineSchematicSymbol] deriving (Eq, Show)

symbolPositionsMap :: Puzzle -> Map.Map Point Char
symbolPositionsMap (Puzzle symbols) =
  foldMap'
    ( \case
        Symbol c p -> Map.singleton p c
        _ -> mempty
    )
    symbols

neightborhoodAround :: Point -> Int -> [Point]
neightborhoodAround (rowStart, colStart) inRowWidth =
  let previousRow = (rowStart - 1,) <$> [colStart - 1 .. colStart + inRowWidth]
      nextRow = (rowStart + 1,) <$> [colStart - 1 .. colStart + inRowWidth]
      adjacent = [(rowStart, colStart - 1), (rowStart, colStart + inRowWidth)]
   in previousRow <> nextRow <> adjacent

sourcePositionToPoint :: SourcePos -> Point
sourcePositionToPoint (SourcePos {sourceLine, sourceColumn}) = (unPos sourceLine, unPos sourceColumn - 1)

numberParser :: Parser EngineSchematicSymbol
numberParser = do
  value <- decimal
  pos <- getSourcePos
  pure $
    Number
      value
      ((+ negate (length (show value) - 1)) <$> sourcePositionToPoint pos)
      (sourcePositionToPoint pos)

dotSymbolParser :: Parser EngineSchematicSymbol
dotSymbolParser = do
  char <- asciiChar
  sourcePosition <- getSourcePos
  pure $ case char of
    '.' -> Dot
    '\n' -> Dot
    c -> Symbol c (sourcePositionToPoint sourcePosition)

pointParser :: Parser EngineSchematicSymbol
pointParser =
  numberParser <|> dotSymbolParser

day3Parser :: Parser Puzzle
day3Parser =
  Puzzle
    <$> (pointParser `sepBy` (void eol <|> void (many ".")))

solver1 :: Puzzle -> Int
solver1 puzz@(Puzzle grid) =
  let symbolLocations = symbolPositionsMap puzz
   in getSum $
        foldMap'
          ( \case
              Number v ps pe -> if any (`elem` Map.keys symbolLocations) (neightborhoodAround ps $ width ps pe) then Sum v else mempty
              _ -> mempty
          )
          grid

solver2 :: Puzzle -> Int
solver2 puzz@(Puzzle grid) =
  let gearSymbolLocations = Map.keys $ Map.filter (== '*') (symbolPositionsMap puzz)
      locationVisits =
        foldl' (Map.unionWith (<>)) Map.empty $
          ( \case
              Number v ps pe ->
                let neighborhood = neightborhoodAround ps $ width ps pe
                 in Map.fromList $ (,[v]) <$> neighborhood
              _ -> Map.empty
          )
            <$> grid
      products =
        ( \p ->
            let visitsFrom = Map.lookup p locationVisits
             in getProduct $ case visitsFrom of
                  Just visits -> if length visits == 2 then foldMap' Product visits else mempty
                  Nothing -> mempty
        )
          <$> gearSymbolLocations
      result =
        getSum $
          foldMap'
            ( \case
                1 -> Sum 0
                n -> Sum n
            )
            products
   in result
