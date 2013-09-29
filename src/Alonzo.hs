module Alonzo where

import           Control.Monad (forever)
import           Control.Monad.State (StateT, evalStateT, lift)
import           Network.IRC

import           Logging
import           IRC

type Trait a = Message -> Alonzo a ()

type Alonzo a = StateT a IRC

alonzo :: UserName -> a -> [Trait a] -> Config -> IO ()
alonzo name brain traits c = run c $ (`evalStateT` brain) $ do
  sendMessage (nick name)
  lift . send $ "USER " ++ name ++ " 0 * :Alonzo - Your friendly IRC bot!"
  forever $ do
    m <- receiveMessage
    sequence_ $ map ($ m) traits

sendMessage :: Message -> Alonzo a ()
sendMessage = lift . send . encode

receiveMessage :: Alonzo a Message
receiveMessage = do
  s <- lift receive
  case decode s of
    Nothing -> logError ("Parsing of " ++ show s ++ " failed!") >> receiveMessage
    Just Message {msg_command = "PING", msg_params = servers} -> sendMessage (pong servers) >> receiveMessage
    Just m -> return m

pong :: [ServerName] -> Message
pong = Message Nothing "PONG"
