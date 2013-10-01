module Main (main) where

import           Data.List
import           Data.Char
import           Control.Monad (when)
import           Control.Concurrent (threadDelay)
import           Control.Monad.State (liftIO)
import           System.Random

import           Util
import           Config
import           Alonzo
import           Brain
import qualified Command
import qualified Match
import           Text.Regex.PCRE

main :: IO ()
main = readConfig >>= alonzo nick brain traits
  where
    nick = "alonzo"
    brain = emptyBrain "alonzo"
    traits = [
        memorizeMyNick
      , memorizeNicks
      , mentionAll
      , opMe
      , greet
      , parrot
      , pretendToBe
      , socialize
      , complainAboutPerfect
      ]

-- | Take a break for 200 to 500 milliseconds and think about it.
thinkAboutIt :: Alonzo a ()
thinkAboutIt = liftIO $ randomRIO (200000, 500000) >>= threadDelay

-- | Makes Alonzo mention all nicks in channel if a message contains @all.
mentionAll :: Trait Brain
mentionAll = Match.message $ \chan _ msg -> do
  when (msg `contains` "@all") $ do
    nicks <- recallNicks chan
    Command.privmsg chan ("^ " ++ unwords nicks)

-- | Makes Alonzo complain on "perfect".
complainAboutPerfect :: Trait Brain
complainAboutPerfect = Match.message $ \chan _ msg -> do
  when (map toLower msg `contains` "perfect") $ do
    thinkAboutIt
    randomChoice messages >>= Command.privmsg chan
  where
    messages = [
        "Perfect you say?  Nothing is perfect!"
      , "Perfect!!!!!!!!!!!!"
      , "Perfect again..."
      ]

-- | Makes Alonzo greet you.
greet :: Trait Brain
greet = Match.message $ \chan you msg -> do
  me <- recallMyNick
  when (msg `contains` me && containsGreeting msg) $ do
    thinkAboutIt
    greeting <- randomChoice greetings
    Command.privmsg chan (capitalize greeting ++ " " ++ you ++ ", nice to meet you!  I'm " ++ me ++ ", your friendly IRC bot!")
  where
    greetings = ["greetings" , "hello" , "hey" , "hi" , "howdy" , "welcome"]
    containsGreeting msg = map toLower msg =~ ("\\b(" ++ intercalate "\\b|\\b" greetings ++ "\\b)")

-- | Makes Alonzo pretend to be somebody else.
pretendToBe :: Trait Brain
pretendToBe = Match.message $ \_ _ msg -> do
  me <- recallMyNick
  when (msg `contains` me) $ do
    case msg =~ "[Pp]retend to be ([A-Za-z0-9]*)" of
      [[_, name]] -> do
        thinkAboutIt
        Command.nick name
      _ -> return ()

parrot :: Trait Brain
parrot = Match.message $ \chan _ msg -> do
  me <- recallMyNick
  when (msg `contains` me && msg =~ "(\\bsays?\\b|\\bthinks?\\b)") $ do
    case msg =~ "\"(.*)\"" of
      [[_, sentence]] -> do
        thinkAboutIt
        Command.privmsg chan sentence
      _ -> return ()

-- | Makes Alonzo op everybody who joins a channel.
opMe :: Trait a
opMe = Match.join $ \name chan -> Command.op chan name

-- | Makes Alonzo accept invitations.
socialize :: Trait a
socialize = Match.invite Command.join
