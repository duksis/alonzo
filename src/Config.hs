module Config (readConfig) where

import           Control.Applicative
import           Data.Maybe
import           Data.Char
import           System.Environment (lookupEnv)
import           Text.Read (readMaybe)
import           Network.Socket (HostName, PortNumber)
import           Data.Default

import           IRC

readHost :: IO (Maybe HostName)
readHost = lookupEnv "ALONZO_HOST"

readPort :: IO (Maybe PortNumber)
readPort = fmap toEnum . (>>= readMaybe) <$> lookupEnv "ALONZO_PORT"

readPassword :: IO (Maybe HostName)
readPassword = lookupEnv "ALONZO_PASSWORD"

readSsl :: IO (Maybe Bool)
readSsl = fmap isTrue <$> lookupEnv "ALONZO_USE_SSL"

readVerify :: IO (Maybe Bool)
readVerify = fmap isTrue <$> lookupEnv "ALONZO_VERIFY_SSL"

isTrue :: String -> Bool
isTrue = (== "true") . map toLower

readConfig :: IO Config
readConfig = do
  mHost <- readHost
  mPort <- readPort
  mPassword <- readPassword
  mSsl <- readSsl
  mVerify <- readVerify
  let c = def
  return c {
      host = fromMaybe (host c) mHost
    , port = fromMaybe (port c) mPort
    , password = mPassword
    , ssl = fromMaybe (ssl c) mSsl
    , verify = fromMaybe (verify c) mVerify
    }
