module Alonzo where

import           Control.Monad (forever)
import           Network.IRC

import           Logging
import           IRC

type Trait = Message -> IRC ()

alonzo :: UserName -> [Trait] -> Config -> IO ()
alonzo name traits c = run c $ do
  sendMessage (nick name)
  send ("USER " ++ name ++ " 0 * :Alonzo - Your friendly IRC bot!")
  forever $ do
    m <- receiveMessage
    sequence_ $ map ($ m) traits

sendMessage :: Message -> IRC ()
sendMessage = send . encode

receiveMessage :: IRC Message
receiveMessage = do
  s <- receive
  case decode s of
    Nothing -> logError ("Parsing of " ++ show s ++ " failed!") >> receiveMessage
    Just Message {msg_command = "PING", msg_params = servers} -> sendMessage (pong servers) >> receiveMessage
    Just m -> return m

pong :: [ServerName] -> Message
pong = Message Nothing "PONG"
