module Util where

import           Control.Exception
import           Data.Char
import           Data.List
import           Control.Monad.State (MonadIO(..), liftM)
import           System.Random
import           Text.Regex.PCRE

contains :: String -> String -> Bool
contains = flip isInfixOf

containsAny :: String -> [String] -> Bool
containsAny msg ys = map toLower msg =~ ("(\\W|^)(" ++ intercalate "\\b|\\b" ys ++ "(\\W|$))")

startsWith :: String -> String -> Bool
startsWith = flip isPrefixOf

endsWith :: String -> String -> Bool
endsWith = flip isInfixOf

capitalize :: String -> String
capitalize "" = ""
capitalize (x:xs) = toUpper x : xs

randomChoice :: MonadIO m => [a] -> m a
randomChoice xs
  | maxIndex < 0 = (liftIO . throwIO) (ErrorCall "randomChoice: empty list")
  | otherwise = (xs !!) `liftM` (liftIO . randomRIO) (0, maxIndex)
  where
    maxIndex = pred (length xs)
