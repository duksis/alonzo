module IRC where

import           Control.Applicative
import           Control.Exception
import           Data.Foldable (forM_)
import           Data.Default
import           Control.Monad.Reader (ReaderT, runReaderT, ask, liftIO)
import           Network.Socket (HostName, PortNumber)
import           Network.Connection

import           Encoding
import           Logging

type IRC = ReaderT Connection IO

data Config = Config {
  host :: HostName
, port :: PortNumber
, password :: Maybe String
, ssl :: Bool
, verify :: Bool
} deriving (Eq, Show)

instance Default Config where
  def = Config {
      host = "localhost"
    , port = 6667
    , password = Nothing
    , ssl = False
    , verify = True
    }

run :: Config -> IRC a -> IO a
run c action = do
  ctx <- initConnectionContext
  bracket (connectTo ctx params) connectionClose (runReaderT (sendPassword >> action))
  where
    params = ConnectionParams {
        connectionHostname  = host c
      , connectionPort      = port c
      , connectionUseSecure = sslSettings
      , connectionUseSocks  = Nothing
      }
    sslSettings
      | ssl c = Just def {settingDisableCertificateValidation = not (verify c)}
      | otherwise = Nothing
    sendPassword = forM_ (password c) (send . ("PASS " ++))

send :: String -> IRC ()
send s = ask >>= \con -> liftIO $ do
  logDebug (">>> " ++ s)
  connectionPut con (encodeUtf8 $ s ++ "\r\n")

receive :: IRC String
receive = ask >>= \con -> liftIO $ do
  s <- decodeUtf8 <$> connectionGetLine 4096 con
  logDebug ("<<< " ++ s)
  return s
