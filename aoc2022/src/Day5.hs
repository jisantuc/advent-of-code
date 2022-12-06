{-# LANGUAGE NamedFieldPuns #-}

module Day5 where

import Control.Monad (replicateM_)
import Control.Monad.ST (ST)
import Data.Foldable (traverse_)
import Lib.Stack (Stack, pop, push)

newtype Puzzle s = Puzzle [Stack s Char] deriving (Eq)

data Instruction = Instruction
  { howMany :: Int,
    from :: Int,
    to :: Int
  }
  deriving (Eq, Show)

step :: Puzzle s -> Instruction -> ST s ()
step (Puzzle stacks) (Instruction {howMany, from, to}) =
  replicateM_
    howMany
    ( do
        fromValue <- pop $ stacks !! (from - 1)
        traverse_ (\v -> push v (stacks !! (to - 1))) fromValue
    )
