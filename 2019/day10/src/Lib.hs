module Lib
  ( someFunc
  ) where

import qualified Data.Map as M

type Coord = (Int, Int)

newtype GridBounds =
  GridBounds (Coord, Coord)

instance Semigroup GridBounds where
  gb1 <> gb2 = undefined

newtype Asteroid =
  Asteroid Coord

newtype Station =
  Station Coord

gridBoundsAsteroids :: [Asteroid] -> GridBounds
gridBoundsAsteroids = undefined

gridBoundsStations :: [Station] -> GridBounds
gridBoundsStations = undefined

gridBounds :: [Asteroid] -> [Station] -> GridBounds
gridBounds asts stats = gridBoundsAsteroids asts <> gridBoundsStations stats

contains :: GridBounds -> Coord -> Bool
contains (GridBounds ((ulX, ulY), (lrX, lrY))) (x, y) =
  ulX <= x && x <= lrX && ulY <= y && y <= lrY

asteroidMap :: [Asteroid] -> M.Map Coord Asteroid
asteroidMap []                     = M.empty
asteroidMap (a@(Asteroid p):rocks) = M.singleton p a <> asteroidMap rocks

findAsteroid :: Coord -> M.Map Coord Asteroid -> Maybe Asteroid
findAsteroid p m = M.lookup p m

someFunc :: IO ()
someFunc = putStrLn "someFunc"
