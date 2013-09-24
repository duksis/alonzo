{-# LANGUAGE OverloadedStrings #-}
module Alonzo.FooSpec (main, spec) where

import           Test.Hspec
import           Alonzo.Foo

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "bar" $ do
    it "equals 1" $ do
      bar `shouldBe` 1
