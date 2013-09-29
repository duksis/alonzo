{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Network.IRC

import           Config
import           Alonzo
import           IRC

main :: IO ()
main = do
  readConfig >>= alonzo "alonzo" [socializer, opMe]

-- | Makes Alonzo op everybody who joins a channel.
opMe :: Message -> IRC ()
opMe m = case m of
  Message {msg_prefix = Just (NickName name _ _), msg_command = "JOIN", msg_params = [chan]} ->
    send $ unwords ["MODE", chan, "+o", name]
  _ -> return ()

-- | Makes Alonzo accept invitations.
socializer :: Message -> IRC ()
socializer m = case m of
  Message {msg_command = "INVITE", msg_params = [_, chan]} -> do
    send $ unwords ["JOIN", chan]
  _ -> return ()
