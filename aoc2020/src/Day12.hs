{-# LANGUAGE OverloadedStrings #-}

module Day12 where

import Data.List (foldl')
import Parser (Parser)
import Text.Megaparsec (sepEndBy, (<|>))
import Text.Megaparsec.Char (char, eol, string)
import Text.Megaparsec.Char.Lexer (decimal)

data MoveDirection = North Int | South Int | East Int | West Int | Forward Int deriving (Eq, Show)

data CardinalDirection = N | S | E | W deriving (Eq, Show)

data Turn = TurnLeft | TurnRight | TurnAround deriving (Eq, Show)

data TurnType = Ninety | OneEighty | TwoSeventy deriving (Eq, Show)

data ShipPosition = ShipPosition
  { facing :: CardinalDirection,
    location :: (Int, Int)
  }
  deriving (Show)

data Instruction
  = Move MoveDirection
  | Steer Turn
  deriving (Eq, Show)

turnParser :: Parser Turn
turnParser = do
  direction <- TurnLeft <$ char 'L' <|> TurnRight <$ char 'R'
  degrees <- Ninety <$ string "90" <|> OneEighty <$ string "180" <|> TwoSeventy <$ string "270"
  pure $ case degrees of
    Ninety -> direction
    OneEighty -> TurnAround
    TwoSeventy -> case direction of
      TurnRight -> TurnLeft
      TurnLeft -> TurnRight
      turn -> turn

moveParser :: (Int -> MoveDirection) -> Char -> Parser MoveDirection
moveParser f c =
  char c *> (f <$> decimal)

instructionParser :: Parser Instruction
instructionParser =
  Steer <$> turnParser
    <|> Move
      <$> ( moveParser North 'N'
              <|> moveParser East 'E'
              <|> moveParser West 'W'
              <|> moveParser South 'S'
              <|> moveParser Forward 'F'
          )

puzzleParser :: Parser [Instruction]
puzzleParser = sepEndBy instructionParser eol

left :: CardinalDirection -> CardinalDirection
left N = W
left W = S
left S = E
left E = N

right :: CardinalDirection -> CardinalDirection
right N = E
right E = S
right S = W
right W = N

aboutFace :: CardinalDirection -> CardinalDirection
aboutFace N = S
aboutFace S = N
aboutFace E = W
aboutFace W = E

moveShip :: ShipPosition -> MoveDirection -> ShipPosition
moveShip (ShipPosition face (x, y)) (Forward n) =
  case face of
    E -> ShipPosition face (x + n, y)
    W -> ShipPosition face (x - n, y)
    N -> ShipPosition face (x, y + n)
    S -> ShipPosition face (x, y - n)
moveShip position@(ShipPosition _ (x, y)) (East n) =
  position {location = (x + n, y)}
moveShip position@(ShipPosition _ (x, y)) (West n) =
  position {location = (x - n, y)}
moveShip position@(ShipPosition _ (x, y)) (North n) =
  position {location = (x, y + n)}
moveShip position@(ShipPosition _ (x, y)) (South n) =
  position {location = (x, y - n)}

steerShip :: ShipPosition -> Turn -> ShipPosition
steerShip position@(ShipPosition facing _) turn =
  case turn of
    TurnRight -> position {facing = right facing}
    TurnLeft -> position {facing = left facing}
    TurnAround -> position {facing = aboutFace facing}

step :: ShipPosition -> Instruction -> ShipPosition
step pos (Steer inst) = steerShip pos inst
step pos (Move inst) = moveShip pos inst

solve :: [Instruction] -> ShipPosition
solve instructions =
  let initial = ShipPosition E (0, 0)
   in foldl' (\pos inst -> step pos inst) initial instructions
