module Match where

import           Network.IRC

join :: Monad m => (UserName -> Channel -> m ()) -> Message -> m ()
join f m = case m of
  Message {msg_prefix = Just (NickName name _ _), msg_command = "JOIN", msg_params = [chan]} -> f name chan
  _ -> return ()

invite :: Monad m => (Channel -> m ()) -> Message -> m ()
invite f m = case m of
  Message {msg_command = "INVITE", msg_params = [_, chan]} -> f chan
  _ -> return ()

message :: Monad m => (Channel -> UserName -> String -> m ()) -> Message -> m ()
message f m = case m of
  Message {msg_prefix = Just (NickName from _ _), msg_command = "PRIVMSG", msg_params = [chan@('#':_), msg]} -> f chan from msg
  _ -> return ()

nick :: Monad m => (UserName -> UserName -> m ()) -> Message -> m ()
nick f m = case m of
  Message {msg_prefix = Just (NickName old _ _), msg_command = "NICK", msg_params = [new]} -> f old new
  _ -> return ()
