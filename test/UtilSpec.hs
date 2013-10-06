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
  describe "contains" $ do
    it "returnd True if string contains word" $ do
      contains "who when why" "when" `shouldBe` True

    it "retirns False if string doesn't contain the word" $ do
      contains "who why" "when" `shouldBe` False

  describe "containsAny" $ do
    it "returnd True if string contains any of given words" $ do
      containsAny "who, when, why" ["when", "where"] `shouldBe` True

    it "returns False if string doesn't contain any of given words" $ do
      containsAny "who, why" ["when"] `shouldBe` False

    it "returnd True if string contains words with nonword characters" $ do
      containsAny "hey all:" ["@all", "all:"] `shouldBe` True

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
