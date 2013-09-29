{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Data.List
import           Data.Default
import           Network.IRC

import           Config
import           Alonzo
import           Brain
import qualified Command

contains :: String -> String -> Bool
contains = flip isInfixOf

main :: IO ()
main = readConfig >>= alonzo "alonzo" def [memorizeNicks, mentionAll, opMe, socialize]

-- | Makes Alonzo mention all nicks in channel if a message contains @all.
mentionAll :: Trait Brain
mentionAll m = case m of
  Message {msg_command = "PRIVMSG", msg_params = [chan@('#':_), msg]} | msg `contains` "@all" -> do
    recallNicks chan >>= Command.privmsg chan . ("^ " ++) . unwords
  _ -> return ()

-- | Makes Alonzo op everybody who joins a channel.
opMe :: Trait a
opMe m = case m of
  Message {msg_prefix = Just (NickName name _ _), msg_command = "JOIN", msg_params = [chan]} ->
    Command.op chan name
  _ -> return ()

-- | Makes Alonzo accept invitations.
socialize :: Trait a
socialize m = case m of
  Message {msg_command = "INVITE", msg_params = [_, chan]} -> do
    Command.join chan
  _ -> return ()
