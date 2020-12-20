{-# LANGUAGE OverloadedStrings #-}

module Day12 where

import Data.List (foldl')
import Parser (Parser)
import Text.Megaparsec (sepEndBy, (<|>))
import Text.Megaparsec.Char (char, eol, string)
import Text.Megaparsec.Char.Lexer (decimal)

data MoveWaypoint
  = North Int
  | South Int
  | East Int
  | West Int
  deriving (Eq, Show)

type FullSteamAhead = Int

data CardinalDirection = N | S | E | W deriving (Eq, Show)

data Rotation = TurnRight Double | TurnLeft Double deriving (Eq, Show)

data ShipPosition = ShipPosition
  { waypointVector :: (Double, Double),
    location :: (Double, Double)
  }
  deriving (Eq, Show)

data Instruction
  = MoveWayPoint MoveWaypoint
  | MoveShip FullSteamAhead
  | RotateWaypoint Rotation
  deriving (Eq, Show)

toRadians :: Int -> Double
toRadians degrees = (fromIntegral degrees) / 360 * (2 * pi)

rotationParser :: Parser Rotation
rotationParser =
  do
    TurnLeft <$> (char 'L' *> (toRadians <$> decimal))
    <|> TurnRight <$> (char 'R' *> (toRadians <$> decimal))

moveParser :: (Int -> MoveWaypoint) -> Char -> Parser MoveWaypoint
moveParser f c =
  char c *> (f <$> decimal)

goForwardParser :: Parser FullSteamAhead
goForwardParser = char 'F' *> decimal

instructionParser :: Parser Instruction
instructionParser =
  RotateWaypoint <$> rotationParser
    <|> MoveWayPoint
      <$> ( moveParser North 'N'
              <|> moveParser East 'E'
              <|> moveParser West 'W'
              <|> moveParser South 'S'
          )
    <|> MoveShip <$> goForwardParser

puzzleParser :: Parser [Instruction]
puzzleParser = sepEndBy instructionParser eol

moveWaypoint :: ShipPosition -> MoveWaypoint -> ShipPosition
moveWaypoint position@(ShipPosition (x, y) _) (East n) =
  position {waypointVector = (x + fromIntegral n, y)}
moveWaypoint position@(ShipPosition (x, y) _) (West n) =
  position {waypointVector = (x - fromIntegral n, y)}
moveWaypoint position@(ShipPosition (x, y) _) (North n) =
  position {waypointVector = (x, y + fromIntegral n)}
moveWaypoint position@(ShipPosition (x, y) _) (South n) =
  position {waypointVector = (x, y - fromIntegral n)}

rotateWaypoint :: ShipPosition -> Rotation -> ShipPosition
rotateWaypoint position@(ShipPosition (wpX, wpY) _) rotation =
  let rotationAngle =
        ( case rotation of
            TurnRight radians -> (2 * pi) - radians
            TurnLeft radians -> radians
        )
      rotatedX = wpX * cos rotationAngle - wpY * sin rotationAngle
      rotatedY = wpX * sin rotationAngle + wpY * cos rotationAngle
   in position
        { waypointVector =
            ( rotatedX,
              rotatedY
            )
        }

moveShip :: ShipPosition -> Int -> ShipPosition
moveShip (ShipPosition waypoint@(wpX, wpY) (shipX, shipY)) n =
  let newShipX = shipX + wpX * fromIntegral n
      newShipY = shipY + wpY * fromIntegral n
   in ShipPosition waypoint (newShipX, newShipY)

step :: ShipPosition -> Instruction -> ShipPosition
step pos (RotateWaypoint inst) = rotateWaypoint pos inst
step pos (MoveShip inst) = moveShip pos inst
step pos (MoveWayPoint inst) = moveWaypoint pos inst

solve :: [Instruction] -> ShipPosition
solve instructions =
  let initial = ShipPosition (10, 1) (0, 0)
   in foldl' (\pos inst -> step pos inst) initial instructions
