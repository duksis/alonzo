{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Monad (when)
import           Data.List
import           Data.Default

import           Config
import           Alonzo
import           Brain
import qualified Command
import qualified Match

contains :: String -> String -> Bool
contains = flip isInfixOf

main :: IO ()
main = readConfig >>= alonzo "alonzo" def [memorizeNicks, mentionAll, opMe, socialize]

-- | Makes Alonzo mention all nicks in channel if a message contains @all.
mentionAll :: Trait Brain
mentionAll = Match.message $ \chan msg -> do
  when (msg `contains` "@all") $ do
    nicks <- recallNicks chan
    Command.privmsg chan ("^ " ++ unwords nicks)

-- | Makes Alonzo op everybody who joins a channel.
opMe :: Trait a
opMe = Match.join $ \name chan -> Command.op chan name

-- | Makes Alonzo accept invitations.
socialize :: Trait a
socialize = Match.invite Command.join
