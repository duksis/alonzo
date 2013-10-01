module Command where

import           Control.Monad.State (lift)
import           Network.IRC (Channel, UserName)
import qualified Network.IRC.Commands as C

import           IRC
import           Alonzo

join :: Channel -> Alonzo a ()
join chan = lift . send $ unwords ["JOIN", chan]

op :: Channel -> UserName -> Alonzo a ()
op chan name = lift . send $ unwords ["MODE", chan, "+o", name]

privmsg :: Channel -> String -> Alonzo a ()
privmsg chan = sendMessage . C.privmsg chan

nick :: UserName -> Alonzo a ()
nick = sendMessage . C.nick
