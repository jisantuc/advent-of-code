module Lib.StackSpec where

import Control.Monad.ST (runST)
import Lib.Stack (empty, pop, push)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Stack" $ do
  it "returns Nothing when popping from an empty stack" $ do
    runST (empty >>= pop) `shouldBe` (Nothing :: Maybe Int)
  it "pops a pushed value from an empty stack" $ do
    runST
      ( do
          v <- empty
          push 'a' v
          pop v
      )
      `shouldBe` Just 'a'
  it "pops two pushed values" $ do
    runST
      ( do
          v <- empty
          push 'a' v
          push 'b' v
          lastIn <- pop v
          firstIn <- pop v
          pure $ (,) <$> lastIn <*> firstIn
      )
      `shouldBe` Just ('b', 'a')
