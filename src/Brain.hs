module Brain (
  Brain
, memorizeNicks
, recallNicks
) where

import           Control.Applicative
import           Data.Monoid
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Default
import           Data.Set (Set)
import qualified Data.Set as Set
import           Network.IRC
import           Control.Monad.State (gets, modify)

import           Alonzo

-- | A simple non-persistent brain.
data Brain = Brain {
  nicks :: Map Channel (Set UserName)
} deriving (Eq, Show)

instance Default Brain where
  def = Brain def

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
