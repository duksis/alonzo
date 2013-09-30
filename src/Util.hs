module Util where

import           Control.Applicative
import           Control.Exception
import           Data.List
import           System.Random

contains :: String -> String -> Bool
contains = flip isInfixOf

randomChoice :: [a] -> IO a
randomChoice xs
  | maxIndex < 0 = throwIO (ErrorCall "randomChoice: empty list")
  | otherwise = (xs !!) <$> randomRIO (0, maxIndex)
  where
    maxIndex = pred (length xs)
