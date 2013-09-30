module Brain (
  Brain
, emptyBrain
, memorizeMyNick
, recallMyNick
, memorizeNicks
, recallNicks
) where

import           Control.Applicative
import           Control.Monad (when)
import           Data.Monoid
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Default
import           Data.Set (Set)
import qualified Data.Set as Set
import           Network.IRC
import           Control.Monad.State (gets, modify)

import           Alonzo
import qualified Match

-- | A simple non-persistent brain.
data Brain = Brain {
  nicks :: Map Channel (Set UserName)
, myNick :: UserName
} deriving (Eq, Show)

emptyBrain :: UserName -> Brain
emptyBrain = Brain def

recallMyNick :: Alonzo Brain UserName
recallMyNick = gets myNick

memorizeMyNick :: Trait Brain
memorizeMyNick = Match.nick $ \old new -> do
  mine <- gets myNick
  when (old == mine) $ do
    modify $ \b -> b{myNick = new}

recallNicks :: Channel -> Alonzo Brain [UserName]
recallNicks c = maybe [] Set.elems . Map.lookup c <$> gets nicks

memorizeNicks :: Trait Brain
memorizeNicks m = case m of
  Message {msg_command = "353", msg_params = [_, _, c, msg]} -> modify setNicks
    where
      names = map stripOp (words msg)
      setNicks b = b{nicks = Map.insertWith mappend c (Set.fromList names) (nicks b)}
      stripOp ('@':name) = name
      stripOp ('+':name) = name
      stripOp name = name
  _ -> return ()
