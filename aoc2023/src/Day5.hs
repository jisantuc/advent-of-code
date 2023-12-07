{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Day5 where

import AoC.Parser (Parser)
import Control.Applicative ((<|>))
import Data.Foldable (foldl')
import Data.Functor (void)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Text.Megaparsec (between, sepBy, sepEndBy)
import Text.Megaparsec.Char (eol, hspace)
import Text.Megaparsec.Char.Lexer (decimal)

data RangeMap = RangeMap
  { sourceStart :: Int,
    destinationStart :: Int,
    mappingLength :: Int
  }
  deriving (Eq, Ord, Show)

elemIn :: Int -> RangeMap -> Maybe Int
elemIn search (RangeMap {sourceStart, destinationStart, mappingLength}) =
  let searchGap = search - sourceStart
   in if searchGap <= mappingLength && searchGap >= 0
        then Just $ [destinationStart ..] !! (search - sourceStart)
        else Nothing

elemForRanges :: [RangeMap] -> Int -> Int
elemForRanges ranges n =
  go ranges n Nothing
  where
    go ranges' search' acc =
      fromMaybe search' $
        foldl' (<|>) acc (elemIn search' <$> ranges')

data Puzzle = Puzzle
  { seedsToPlant :: [Int],
    seedToSoilRanges :: [RangeMap],
    soilToFertilizerRanges :: [RangeMap],
    fertilizerToWaterRanges :: [RangeMap],
    waterToLightRanges :: [RangeMap],
    lightToTemperatureRanges :: [RangeMap],
    temperatureToHumidityRanges :: [RangeMap],
    humidityToLocationRanges :: [RangeMap]
  }
  deriving (Eq, Show)

rangeMapParser :: Parser RangeMap
rangeMapParser = do
  destinationStart <- decimal <* hspace
  sourceStart <- decimal <* hspace
  mappingLength <- decimal
  pure $ RangeMap {sourceStart, destinationStart, mappingLength}

parser :: Parser Puzzle
parser = do
  seedsToPlant <- "seeds: " *> decimal `sepBy` hspace
  void $ eol *> eol
  seedToSoilRanges <- "seed-to-soil map:" *> eol *> rangeMapParser `sepEndBy` eol
  void eol
  soilToFertilizerRanges <- "soil-to-fertilizer map:" *> eol *> rangeMapParser `sepEndBy` eol
  fertilizerToWaterRanges <- eol *> "fertilizer-to-water map:" *> eol *> rangeMapParser `sepEndBy` eol
  waterToLightRanges <- eol *> "water-to-light map:" *> eol *> rangeMapParser `sepEndBy` eol
  lightToTemperatureRanges <- eol *> "light-to-temperature map:" *> eol *> rangeMapParser `sepEndBy` eol
  temperatureToHumidityRanges <- eol *> "temperature-to-humidity map:" *> eol *> rangeMapParser `sepEndBy` eol
  humidityToLocationRanges <- eol *> "humidity-to-location map:" *> eol *> (rangeMapParser `sepBy` eol)
  pure $
    Puzzle
      { seedsToPlant,
        seedToSoilRanges,
        soilToFertilizerRanges,
        fertilizerToWaterRanges,
        waterToLightRanges,
        lightToTemperatureRanges,
        temperatureToHumidityRanges,
        humidityToLocationRanges
      }

rangesParser :: Parser Text -> Parser [RangeMap]
rangesParser p = between (eol *> p *> eol) eol (rangeMapParser `sepBy` eol)

converter :: (Puzzle -> [RangeMap]) -> Puzzle -> Int -> Int
converter f puzzle = elemForRanges (f puzzle)

soilForSeed :: Puzzle -> Int -> Int
soilForSeed = converter seedToSoilRanges

fertilizerForSoil :: Puzzle -> Int -> Int
fertilizerForSoil = converter soilToFertilizerRanges

waterForFertilizer :: Puzzle -> Int -> Int
waterForFertilizer = converter fertilizerToWaterRanges

lightForWater :: Puzzle -> Int -> Int
lightForWater = converter waterToLightRanges

temperatureForLight :: Puzzle -> Int -> Int
temperatureForLight = converter lightToTemperatureRanges

humidityForTemperature :: Puzzle -> Int -> Int
humidityForTemperature = converter temperatureToHumidityRanges

locationForHumidity :: Puzzle -> Int -> Int
locationForHumidity = converter humidityToLocationRanges

locationForSeed :: Puzzle -> Int -> Int
locationForSeed p =
  locationForHumidity p
    . humidityForTemperature p
    . temperatureForLight p
    . lightForWater p
    . waterForFertilizer p
    . fertilizerForSoil p
    . soilForSeed p

solver1 :: Puzzle -> Int
solver1 puzz@(Puzzle {seedsToPlant}) =
  minimum (locationForSeed puzz <$> seedsToPlant)
