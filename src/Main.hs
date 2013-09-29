{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Data.List
import           Data.Default
import           Control.Monad.State (lift)
import           Network.IRC

import           Config
import           Alonzo
import           Brain
import           IRC

contains :: String -> String -> Bool
contains = flip isInfixOf

main :: IO ()
main = readConfig >>= alonzo "alonzo" def [memorizeNicks, mentionAll, opMe, socialize]

-- | Makes Alonzo mention all nicks in channel if a message contains @all.
mentionAll :: Trait Brain
mentionAll m = case m of
  Message {msg_command = "PRIVMSG", msg_params = [chan@('#':_), msg]} | msg `contains` "@all" -> do
    recallNicks chan >>= sendMessage . privmsg chan . ("^ " ++) . unwords
  _ -> return ()

-- | Makes Alonzo op everybody who joins a channel.
opMe :: Trait a
opMe m = case m of
  Message {msg_prefix = Just (NickName name _ _), msg_command = "JOIN", msg_params = [chan]} ->
    lift . send $ unwords ["MODE", chan, "+o", name]
  _ -> return ()

-- | Makes Alonzo accept invitations.
socialize :: Trait a
socialize m = case m of
  Message {msg_command = "INVITE", msg_params = [_, chan]} -> do
    lift . send $ unwords ["JOIN", chan]
  _ -> return ()
