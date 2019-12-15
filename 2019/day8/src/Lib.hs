module Lib
  ( rastersParser
  , getRasterStats
  , combineRasters
  , resolveRow
  , Raster(..)
  , RasterRow(..)
  , InterleavedRow(..)
  , RasterBit(..)
  ) where

import           Control.Applicative              ((<|>))
import           Control.Monad                    (replicateM)
import           Data.Attoparsec.ByteString.Char8
import           Data.Monoid                      (Sum (..))

data RasterBit
  = Zero
  | One
  | Two
  deriving (Eq)

instance Show RasterBit where
  show Zero = " "
  show One  = "1"
  show Two  = " "

instance Semigroup RasterBit where
  Two <> b = b
  b <> Two = b
  l <> _ = l

instance Monoid RasterBit where
  mempty = Zero

newtype RasterRow =
  RasterRow [RasterBit]
  deriving (Eq)

instance Show RasterRow where
  show (RasterRow bs) = mconcat (show <$> bs)

newtype InterleavedRow =
  InterleavedRow [[RasterBit]]
  deriving (Eq, Show)

instance Semigroup InterleavedRow where
  (InterleavedRow xs) <> (InterleavedRow ys) =
    InterleavedRow $ zipWith (++) xs ys

data Raster
  = Raster [RasterRow]
  | InterleavedRaster [InterleavedRow]
  deriving (Eq, Show)

rasterBit :: Parser RasterBit
rasterBit =
  const Zero <$> char '0' <|> const One <$> char '1' <|> const Two <$> char '2'

rasterRowParser :: Parser RasterRow
rasterRowParser = RasterRow <$> replicateM 25 rasterBit

rasterParser :: Parser Raster
rasterParser = Raster <$> replicateM 6 rasterRowParser

rastersParser :: Parser [Raster]
rastersParser = do
  rast <- rasterParser
  (const (rast : []) <$> (skipSpace *> endOfInput)) <|>
    (rast :) <$> rastersParser

getRowStats :: RasterRow -> (Sum Int, Sum Int, Sum Int)
getRowStats (RasterRow []) = (Sum 0, Sum 0, Sum 0)
getRowStats (RasterRow (Zero:bs)) =
  (Sum 1, Sum 0, Sum 0) <> getRowStats (RasterRow bs)
getRowStats (RasterRow (One:bs)) =
  (Sum 0, Sum 1, Sum 0) <> getRowStats (RasterRow bs)
getRowStats (RasterRow (Two:bs)) =
  (Sum 0, Sum 0, Sum 1) <> getRowStats (RasterRow bs)

-- being lazy with second implementation because I don't call it and this is for puzzles
getRasterStats :: Raster -> (Int, Int)
getRasterStats (Raster rast) =
  let (zeros, ones, twos) = mconcat $ getRowStats <$> rast
   in (getSum zeros, (getSum ones * getSum twos))
getRasterStats (InterleavedRaster _) = undefined

toInterleavedRow :: RasterRow -> InterleavedRow
toInterleavedRow (RasterRow bs) = InterleavedRow $ pure <$> bs

combineRasterRow :: RasterRow -> RasterRow -> InterleavedRow
combineRasterRow (RasterRow xs) (RasterRow ys) =
  InterleavedRow $ (\x -> [fst x, snd x]) <$> zip xs ys

combineRasters :: Raster -> Raster -> Raster
combineRasters (Raster r1) (Raster r2) =
  InterleavedRaster $ zipWith combineRasterRow r1 r2
combineRasters (InterleavedRaster r1) (InterleavedRaster r2) =
  InterleavedRaster $ zipWith (<>) r1 r2
combineRasters (Raster r1) r2@(InterleavedRaster _) =
  combineRasters (InterleavedRaster $ toInterleavedRow <$> r1) r2
combineRasters r1@(InterleavedRaster _) (Raster r2) =
  combineRasters r1 (InterleavedRaster $ toInterleavedRow <$> r2)

resolveRow :: InterleavedRow -> RasterRow
resolveRow (InterleavedRow bs) = RasterRow $ mconcat <$> bs
