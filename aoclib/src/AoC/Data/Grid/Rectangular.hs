{-# LANGUAGE NoImplicitPrelude #-}

module AoC.Data.Grid.Rectangular where

import Prelude
  ( Int,
    abs,
    (+),
    (-),
  )

type Point = (Int, Int)

data Direction
  = Right Int
  | Left Int
  | Up Int
  | Down Int

manhattanDistance :: Point -> Point -> Int
manhattanDistance (x1, y1) (x2, y2) =
  abs (x1 - x2) + abs (y1 - y2)
