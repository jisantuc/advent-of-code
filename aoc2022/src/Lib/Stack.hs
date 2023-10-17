module Lib.Stack where

import Control.Monad (replicateM)
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.List as List
import Data.Maybe (catMaybes, isNothing)
import Data.Vector (Vector, lastM, snoc, unsnoc)
import qualified Data.Vector as Vector

newtype Stack a = Stack (IORef (Vector a)) deriving (Eq)

push :: a -> Stack a -> IO ()
push item (Stack ref) = readIORef ref >>= writeIORef ref . (`snoc` item)

pop :: Stack a -> IO (Maybe a)
pop (Stack ref) = do
  v <- readIORef ref
  traverse (\(h, t) -> writeIORef ref h >> pure t) (unsnoc v)

popSeveral :: Int -> Stack a -> IO [a]
popSeveral howMany stack = do
  reverse . catMaybes <$> replicateM howMany (pop stack)

top :: Stack a -> IO (Maybe a)
top (Stack ref) = lastM <$> readIORef ref

empty :: IO (Stack a)
empty = Stack <$> newIORef Vector.empty

ofAs :: (Traversable t) => t a -> IO (Stack a)
ofAs items = do
  stack <- empty
  traverse_ (`push` stack) items
  pure stack

key :: (Show a) => Stack a -> String -> IO String
key (Stack ref) separator =
  readIORef ref <&> \vec ->
    let asList = show <$> Vector.toList vec
        withSeparator = if List.null asList then ["/"] else List.intersperse separator asList
     in mconcat withSeparator

null :: Stack a -> IO Bool
null = (isNothing <$>) . top
