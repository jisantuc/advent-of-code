module Lib.StackSpec where

import Lib.Stack (empty, pop, push, top)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Stack" $ do
  it "returns Nothing when popping from an empty stack" $
    (empty >>= pop) >>= (`shouldBe` (Nothing :: Maybe Int))
  it "pops a pushed value from an empty stack" $
    ( do
        v <- empty
        push 'a' v
        pop v
    )
      >>= (`shouldBe` Just 'a')
  it "pops two pushed values" $
    do
      v <- empty
      push 'a' v
      push 'b' v
      lastIn <- pop v
      firstIn <- pop v
      (,) <$> lastIn <*> firstIn `shouldBe` Just ('b', 'a')
  it "reads the top value without mutation" $
    do
      v <- empty
      push 'a' v
      push 'b' v
      lastIn <- top v
      lastInAgain <- top v
      (,) <$> lastIn <*> lastInAgain `shouldBe` Just ('b', 'b')
