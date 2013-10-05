{-# LANGUAGE ScopedTypeVariables #-}
module UtilSpec (main, spec) where

import           Helper
import           Control.Monad (replicateM)
import           Data.List (nub, sort)

import           Util

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "randomChoice" $ do
    it "returns a random element of a list" $ do
      property $ \(NonEmpty xs :: NonEmptyList Int) -> do
        x <- randomChoice xs
        x `shouldSatisfy` (`elem` xs)

    it "returns every element of the list eventually" $ do
      let xs = [10, 20, 30, 23, 42] :: [Int]
      ys <- replicateM 100 (randomChoice xs)
      sort xs `shouldBe` (sort . nub) ys

    context "when used with an empty list" $ do
      it "throws an exception" $ do
        randomChoice [] `shouldThrow` errorCall "randomChoice: empty list"
