module Lib.Stack where

import Control.Monad.ST (ST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Vector (Vector, snoc, unsnoc)
import qualified Data.Vector as Vector

newtype Stack s a = Stack (STRef s (Vector a)) deriving (Eq)

push :: a -> Stack s a -> ST s ()
push item (Stack ref) = readSTRef ref >>= writeSTRef ref . (`snoc` item)

pop :: Stack s a -> ST s (Maybe a)
pop (Stack ref) = do
  v <- readSTRef ref
  traverse (\(h, t) -> writeSTRef ref h >> pure t) (unsnoc v)

empty :: ST s (Stack s a)
empty = Stack <$> newSTRef Vector.empty
