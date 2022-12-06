{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Day5Spec where

import Data.Text (Text)
import Data.Void (Void)
import Test.Hspec (Spec, describe)
import Text.Megaparsec (ParseErrorBundle)
import Text.RawString.QQ (r)

spec :: Spec
spec = describe "day 5" $ pure ()

parsedExample :: Either (ParseErrorBundle Void Text) Int
parsedExample = undefined

examplePuzzle :: Text
examplePuzzle =
  [r|    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
|]
