module Lib
    ( rastersParser
    , getRasterStats
    ) where

import           Control.Applicative              ((<|>))
import           Control.Monad                    (replicateM)
import           Data.Attoparsec.ByteString.Char8
import           Data.Monoid                      (Sum (..))

data RasterBit = Zero
  | One
  | Two deriving (Eq, Show)

type RasterRow = [RasterBit]
type Raster = [RasterRow]

fromChar :: Char -> RasterBit
fromChar '0' = Zero
fromChar '1' = One
fromChar '2' = Two
fromChar c   = error $ "Invalid raster bit: " ++ show c

rasterBit :: Parser RasterBit
rasterBit = fromChar <$> (char '0' <|> char '1' <|> char '2')

rasterRowParser :: Parser RasterRow
rasterRowParser = replicateM 25 rasterBit

rasterParser :: Parser Raster
rasterParser = replicateM 6 rasterRowParser

rastersParser :: Parser [Raster]
rastersParser = do
  rast <- rasterParser
  (const (rast : []) <$> (skipSpace *> endOfInput)) <|> (rast :) <$> rastersParser

getRowStats :: RasterRow -> (Sum Int, Sum Int, Sum Int)
getRowStats []        = (Sum 0, Sum 0, Sum 0)
getRowStats (Zero:bs) = (Sum 1, Sum 0, Sum 0) <> getRowStats bs
getRowStats (One:bs)  = (Sum 0, Sum 1, Sum 0) <> getRowStats bs
getRowStats (Two:bs)  = (Sum 0, Sum 0, Sum 1) <> getRowStats bs

getRasterStats :: Raster -> (Int, Int)
getRasterStats rast =
  let
    (zeros, ones, twos) = mconcat $ getRowStats <$> rast
  in
    (getSum zeros, (getSum ones * getSum twos))
