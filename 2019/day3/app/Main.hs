module Main where

import           Control.Monad.Trans.State
import           Data.Sequence             (Seq (..))
import           Lib

instructions :: [Instruction]
instructions =
  [Right' 4, Left' 4]

main :: IO ()
main =
  let calc = execState $ track instructions
  in
    print . show $ calc Empty
