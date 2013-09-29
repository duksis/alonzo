module Logging where

import           Control.Monad.Reader (MonadIO(..))

data LogLevel = DEBUG | INFO | WARN | ERROR
  deriving (Eq, Show)

logDebug :: MonadIO m => String -> m ()
logDebug = logMessage DEBUG

logInfo :: MonadIO m => String -> m ()
logInfo = logMessage INFO

logWarn :: MonadIO m => String -> m ()
logWarn = logMessage WARN

logError :: MonadIO m => String -> m ()
logError = logMessage ERROR

logMessage :: MonadIO m => LogLevel -> String -> m ()
logMessage level msg = liftIO $ putStrLn ("[" ++ show level ++ "] " ++ msg)
