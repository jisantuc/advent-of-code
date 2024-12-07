{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Day6 where

import AoC.Data.Grid.Parser (sourcePositionToPoint)
import AoC.Data.Grid.Rectangular
  ( Direction (..),
    Point,
    RectangularGrid (..),
    debugShow,
    replace,
  )
import qualified AoC.Data.Grid.Rectangular as RectangularGrid
import AoC.Parser (Parser, fromMap)
import Data.Foldable (foldl')
import qualified Data.Map as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Vector ((!))
import qualified Data.Vector as Vector
import Debug.Trace (traceShow)
import Text.Megaparsec (getSourcePos, many, sepBy)
import Text.Megaparsec.Char (eol)

data FloorSpace = Open | Obstacle | Guard (Direction ()) deriving (Eq, Show)

data GuardPath
  = GuardPath [(Point, Direction ())]
  | Loop [(Point, Direction ())]
  | DeadEnd [(Point, Direction ())]
  deriving (Eq, Show)

guardPathToList :: GuardPath -> [(Point, Direction ())]
guardPathToList (GuardPath xs) = xs
guardPathToList (Loop xs) = xs
guardPathToList (DeadEnd xs) = xs

isLoop :: GuardPath -> Bool
isLoop (Loop _) = True
isLoop _ = False

isGuard :: (a, FloorSpace) -> Bool
isGuard (_, Guard _) = True
isGuard _ = False

spaceParser :: Parser ((Int, Int), FloorSpace)
spaceParser = do
  pos <- sourcePositionToPoint <$> getSourcePos
  space <- fromMap $ Map.fromList [(".", Open), ("#", Obstacle), ("^", Guard (Up ()))]
  pure (pos, space)

puzzleParser :: Parser (RectangularGrid ((Int, Int), FloorSpace))
puzzleParser =
  RectangularGrid.fromLists <$> many spaceParser `sepBy` eol

debugGrid :: Set.Set Point -> RectangularGrid (Point, FloorSpace) -> String
debugGrid visited =
  debugShow
    ( \case
        (_, Obstacle) -> '#'
        (p, _) -> if p `elem` visited then 'X' else '.'
    )

debugGridWithObstacles :: Set.Set Point -> Set.Set Point -> RectangularGrid (Point, FloorSpace) -> String
debugGridWithObstacles extraObstacles visited =
  ("\n" <>)
    . debugShow
      ( \case
          (p, _) | p `elem` extraObstacles -> 'O'
          (_, Obstacle) -> '#'
          (_, Guard _) -> '^'
          (p, _) | p `elem` visited -> 'X'
          _ -> '.'
      )

moveRight :: (Num b) => (a, b) -> ((a, b), Direction ())
moveRight (row, col) = ((row, col + 1), RectangularGrid.Right ())

moveLeft :: (Num b) => (a, b) -> ((a, b), Direction ())
moveLeft (row, col) = ((row, col - 1), RectangularGrid.Left ())

moveDown :: (Num a) => (a, b) -> ((a, b), Direction ())
moveDown (row, col) = ((row + 1, col), RectangularGrid.Down ())

moveUp :: (Num a) => (a, b) -> ((a, b), Direction ())
moveUp (row, col) = ((row - 1, col), RectangularGrid.Up ())

mapGuardPath :: RectangularGrid ((Int, Int), FloorSpace) -> GuardPath
mapGuardPath (RectangularGrid mat) = visited
  where
    guards = Vector.find isGuard <$> mat
    guard =
      Vector.find isJust guards >>= \case
        Nothing -> Nothing
        Just (_, Open) -> Nothing
        Just (_, Obstacle) -> Nothing
        Just (p, Guard dir) -> Just (p, dir)
    nCols = length $ Vector.head mat
    nRows = length mat
    visited = case guard of
      Nothing -> GuardPath []
      Just (p, dir) -> go [] (p, dir)
    go acc pair@(p, _) =
      if pair `elem` acc
        then Loop acc
        else
          let acced = acc <> [pair]
           in case pair of
                ((row, col), RectangularGrid.Right _) ->
                  -- if at edge, leave
                  if col == nCols - 1
                    then GuardPath acced
                    else
                      -- if unobstructed, keep going
                      if snd (mat ! row ! (col + 1)) /= Obstacle
                        then go acced (moveRight p)
                        else
                          -- if turning right to avoid an obstacle on the bottom row, leave
                          if row == nRows - 1
                            then GuardPath acced
                            else
                              if snd (mat ! (row + 1) ! col) /= Obstacle
                                then go acced (moveDown p)
                                else go acced (moveLeft p)
                ((row, col), RectangularGrid.Left _) ->
                  if col == 0
                    then GuardPath acced
                    else
                      if snd (mat ! row ! (col - 1)) /= Obstacle
                        then go acced (moveLeft p)
                        else
                          if row == 0
                            then GuardPath acced
                            else
                              if snd (mat ! (row - 1) ! col) /= Obstacle
                                then go acced (moveUp p)
                                else go acced (moveRight p)
                ((row, col), RectangularGrid.Down _) ->
                  if row == nRows - 1
                    then GuardPath acced
                    else
                      if snd (mat ! (row + 1) ! col) /= Obstacle
                        then go acced (moveDown p)
                        else
                          if col == 0
                            then GuardPath acced
                            else
                              if snd (mat ! row ! (col - 1)) /= Obstacle
                                then go acced (moveLeft p)
                                else go acced (moveUp p)
                ((row, col), RectangularGrid.Up _) ->
                  if row == 0
                    then GuardPath acced
                    else
                      if snd (mat ! (row - 1) ! col) /= Obstacle
                        then go acced (moveUp p)
                        else
                          if col == nCols - 1
                            then GuardPath acced
                            else
                              if snd (mat ! row ! (col + 1)) /= Obstacle
                                then go acced (moveRight p)
                                else go acced (moveDown p)

solve1 :: RectangularGrid ((Int, Int), FloorSpace) -> Int
solve1 g = case mapGuardPath g of
  DeadEnd _ -> 0
  Loop _ -> 0
  GuardPath visited -> Set.size . Set.fromList $ fst <$> visited

-- if placing an obstacle ahead of the current location causes a turn that leads to
-- any space we've already visited... in the same direction?
solve2 :: RectangularGrid ((Int, Int), FloorSpace) -> Int
solve2 g@(RectangularGrid mat) =
  let guardPath = mapGuardPath g
      guards = Vector.find isGuard <$> mat
      initialGuard =
        Vector.find isJust guards >>= \case
          Nothing -> Nothing
          Just (_, Open) -> Nothing
          Just (_, Obstacle) -> Nothing
          Just (p, Guard dir) -> Just (p, dir)
   in case (initialGuard, guardPath) of
        (_, DeadEnd _) -> 0
        (_, Loop _) -> 0
        (_, GuardPath []) -> 0
        (Nothing, _) -> 0
        (Just (guardPoint, _), GuardPath locations) ->
          Set.size $
            foldl'
              ( \goodObstacles loc ->
                  case checkForLoop g guardPath guardPoint loc of
                    Just p -> Set.insert p goodObstacles
                    Nothing -> goodObstacles
              )
              Set.empty
              (Set.fromList . (fst <$>) $ locations)

checkForLoop :: RectangularGrid ((Int, Int), FloorSpace) -> GuardPath -> Point -> Point -> Maybe Point
checkForLoop g@(RectangularGrid mat) path guardPoint loc =
  let locations = guardPathToList path
      nRows = Vector.length mat
      nCols = Vector.length (mat ! 0)
      withObstacle = replace g loc (loc, Obstacle)
   in traceShow loc $ case loc of
        (r, c) | r == nRows || c == nCols -> Nothing
        p | p `notElem` Set.fromList (fst <$> locations) -> Nothing
        p | p == guardPoint -> Nothing
        _ ->
          if isLoop (mapGuardPath withObstacle)
            then Just loc
            else Nothing
