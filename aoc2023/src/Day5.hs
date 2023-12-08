{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Day5 where

import AoC.Data.Interval (Interval (..))
import qualified AoC.Data.Interval as Interval
import AoC.Parser (Parser)
import Control.Applicative ((<|>))
import Data.Foldable (foldl')
import Data.Functor (void, (<&>))
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Debug.Trace (traceShow)
import Text.Megaparsec (between, eof, sepBy, sepEndBy)
import Text.Megaparsec.Char (eol, hspace)
import Text.Megaparsec.Char.Lexer (decimal)

data RangeMap = RangeMap
  { sourceStart :: Integer,
    destinationStart :: Integer,
    mappingLength :: Integer
  }
  deriving (Eq, Ord, Show)

-- | convert from the source to the destinsion
elemTo :: Integer -> RangeMap -> Maybe Integer
elemTo search (RangeMap {sourceStart, destinationStart, mappingLength}) =
  let searchGap = search - sourceStart
   in if searchGap <= mappingLength && searchGap >= 0
        then Just $ destinationStart + (search - sourceStart)
        else Nothing

-- | convert from the destination to the source
elemFrom :: Integer -> RangeMap -> Maybe Integer
elemFrom search (RangeMap {sourceStart, destinationStart, mappingLength}) =
  let searchGap = search - destinationStart
   in traceShow
        ( "Search gap: "
            <> show searchGap
            <> " source start "
            <> show sourceStart
            <> " dest start "
            <> show destinationStart
            <> " mapping length "
            <> show mappingLength
        )
        $ if searchGap <= mappingLength && searchGap >= 0
          then Just $ sourceStart + searchGap
          else Nothing

invertRangeMap :: RangeMap -> RangeMap
invertRangeMap rm@(RangeMap {sourceStart, destinationStart}) =
  rm {destinationStart = sourceStart, sourceStart = destinationStart}

elemForRanges :: [RangeMap] -> Integer -> Integer
elemForRanges ranges n =
  go ranges n Nothing
  where
    go ranges' search' acc =
      fromMaybe search' $
        foldl' (<|>) acc (elemTo search' <$> ranges')

invertElemFromRanges :: [RangeMap] -> Integer -> Integer
invertElemFromRanges ranges = elemForRanges (invertRangeMap <$> ranges)

-- TODO I think I need a sparse representation of seedsToPlant uh oh
data Puzzle = Puzzle
  { seedsToPlant :: [Interval Integer],
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

seedsParser1 :: Parser [Interval Integer]
seedsParser1 = "seeds: " *> (decimal <&> \d -> Interval d d) `sepBy` hspace

seedsParser2 :: Parser [Interval Integer]
seedsParser2 =
  "seeds: " *> decimal `sepBy` hspace <&> \nums ->
    let pairsOf (x' : x'' : xs) = Interval x' x'' : pairsOf xs
        pairsOf _ = []
        filledInRanges = pairsOf nums
     in filledInRanges

parser :: Parser [Interval Integer] -> Parser Puzzle
parser seedsParser = do
  seedsToPlant <- seedsParser
  void $ eol *> eol
  seedToSoilRanges <- "seed-to-soil map:" *> eol *> rangeMapParser `sepEndBy` eol
  void eol
  soilToFertilizerRanges <- "soil-to-fertilizer map:" *> eol *> rangeMapParser `sepEndBy` eol
  fertilizerToWaterRanges <- eol *> "fertilizer-to-water map:" *> eol *> rangeMapParser `sepEndBy` eol
  waterToLightRanges <- eol *> "water-to-light map:" *> eol *> rangeMapParser `sepEndBy` eol
  lightToTemperatureRanges <- eol *> "light-to-temperature map:" *> eol *> rangeMapParser `sepEndBy` eol
  temperatureToHumidityRanges <- eol *> "temperature-to-humidity map:" *> eol *> rangeMapParser `sepEndBy` eol
  humidityToLocationRanges <- eol *> "humidity-to-location map:" *> eol *> rangeMapParser `sepEndBy` eol
  eof
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

converter :: (Puzzle -> [RangeMap]) -> Puzzle -> Integer -> Integer
converter f puzzle = elemForRanges (f puzzle)

reverseConverter :: (Puzzle -> [RangeMap]) -> Puzzle -> Integer -> Integer
reverseConverter f = converter ((invertRangeMap <$>) . f)

soilForSeed :: Puzzle -> Integer -> Integer
soilForSeed = converter seedToSoilRanges

fertilizerForSoil :: Puzzle -> Integer -> Integer
fertilizerForSoil = converter soilToFertilizerRanges

waterForFertilizer :: Puzzle -> Integer -> Integer
waterForFertilizer = converter fertilizerToWaterRanges

lightForWater :: Puzzle -> Integer -> Integer
lightForWater = converter waterToLightRanges

temperatureForLight :: Puzzle -> Integer -> Integer
temperatureForLight = converter lightToTemperatureRanges

humidityForTemperature :: Puzzle -> Integer -> Integer
humidityForTemperature = converter temperatureToHumidityRanges

locationForHumidity :: Puzzle -> Integer -> Integer
locationForHumidity = converter humidityToLocationRanges

seedForSoil :: Puzzle -> Integer -> Integer
seedForSoil = reverseConverter seedToSoilRanges

soilForFertilizer :: Puzzle -> Integer -> Integer
soilForFertilizer = reverseConverter soilToFertilizerRanges

fertilizerForWater :: Puzzle -> Integer -> Integer
fertilizerForWater = reverseConverter fertilizerToWaterRanges

waterForLight :: Puzzle -> Integer -> Integer
waterForLight = reverseConverter waterToLightRanges

lightForTemperature :: Puzzle -> Integer -> Integer
lightForTemperature = reverseConverter lightToTemperatureRanges

temperatureForHumidity :: Puzzle -> Integer -> Integer
temperatureForHumidity = reverseConverter temperatureToHumidityRanges

humidityForLocation :: Puzzle -> Integer -> Integer
humidityForLocation = reverseConverter humidityToLocationRanges

locationForSeed :: Puzzle -> Integer -> Integer
locationForSeed p =
  locationForHumidity p
    . humidityForTemperature p
    . temperatureForLight p
    . lightForWater p
    . waterForFertilizer p
    . fertilizerForSoil p
    . soilForSeed p

seedForLocation :: Puzzle -> Integer -> Integer
seedForLocation p n =
  let f =
        seedForSoil p
          . soilForFertilizer p
          . fertilizerForWater p
          . waterForLight p
          . lightForTemperature p
          . temperatureForHumidity p
          . humidityForLocation p
   in if snd (n `divMod` 1000000) == 0 then traceShow ("Searching for " <> show n) $ f n else f n

solver1 :: Puzzle -> Integer
solver1 puzz@(Puzzle {seedsToPlant}) =
  minimum (locationForSeed puzz <$> (start <$> seedsToPlant))

-- need to check 0 -> min location in humidityToLocationRanges
-- for whether it exists back to a number in the seed list
solver2 :: Puzzle -> Integer
solver2 puzz@(Puzzle {seedsToPlant, humidityToLocationRanges}) =
  fromMaybe 0
    $ foldl'
      ( \acc range ->
          case acc of
            Just n -> Just n
            Nothing -> go seedsToPlant range
      )
      Nothing
    $ sortBy (\rm1 rm2 -> destinationStart rm1 `compare` destinationStart rm2) humidityToLocationRanges
  where
    go intervals (RangeMap {destinationStart, mappingLength}) =
      traceShow ("Starting interval from " <> show destinationStart <> " to " <> show (destinationStart + mappingLength)) $
        go' intervals destinationStart (destinationStart + mappingLength)
    go' intervals n end =
      let seed = seedForLocation puzz n
       in if any (Interval.elem seed) intervals then Just n else if (n + 1) > end then Nothing else go' intervals (n + 1) end
