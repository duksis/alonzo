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

message :: Monad m => (Channel -> String -> m ()) -> Message -> m ()
message f m = case m of
  Message {msg_command = "PRIVMSG", msg_params = [chan@('#':_), msg]} -> f chan msg
  _ -> return ()
