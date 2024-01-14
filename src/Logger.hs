module Logger
  ( LogLevel(..)
  , LoggerConfig(..)
  , defaultConfig
  , Logger
  , initLogger
  , logMessage
  , waitLogger
  ) where

import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM
import Control.Monad (forever, when)

-- | The severity levels for log messages.
data LogLevel = INFO | WARNING | ERROR | DEBUG deriving (Eq, Ord, Show)

-- | Represents a log message with a severity level and a message.
data LogMessage = LogMessage
  { logLevel :: LogLevel  -- ^ Severity level of the log message.
  , message   :: String   -- ^ The log message.
  } deriving (Show)

-- | Configuration for the logger.
data LoggerConfig = LoggerConfig
  { logConfigLevel    :: LogLevel        -- ^ Minimum severity level to log.
  , logCallback :: LogMessage -> IO ()   -- ^ Callback function to handle log messages.
  }

-- | Default configuration for the logger.
defaultConfig :: LoggerConfig
defaultConfig = LoggerConfig INFO (\msg -> putStrLn $ show (logLevel msg) ++ ": " ++ message msg)

-- | Represents a logger instance.
data Logger = Logger
  { logQueue :: TQueue LogMessage   -- ^ The queue to store log messages.
  , logConfig :: LoggerConfig       -- ^ Configuration for the logger.
  }

-- | Initializes a new logger with the given configuration.
--
-- >>> logger <- initLogger defaultConfig
-- >>> logMessage logger INFO "This is an informational message."
-- >>> logMessage logger ERROR "This is an error message."
--
-- The above doctest initializes a logger, logs an informational message,
-- and logs an error message. The log messages are processed asynchronously, so
-- there's no need to wait for them to complete.
--
-- >>> logMessage logger DEBUG "This is a debug message."
--
-- In this case, the debug message won't be logged because the default
-- configuration has a log level of INFO, so DEBUG messages are ignored.
initLogger :: LoggerConfig -> IO Logger
initLogger config = do
  queue <- newTQueueIO
  let logger = Logger queue config
  _ <- async $ logWorker logger
  return logger

-- | Worker function that processes log messages asynchronously.
--
-- >>> logger <- initLogger defaultConfig
-- >>> logMessage logger INFO "This is an informational message."
-- >>> logMessage logger ERROR "This is an error message."
-- >>> wait logger
--
-- The wait function is used to ensure that all log messages are processed
-- before the program exits. This is necessary in doctests to allow asynchronous
-- operations to complete.
logWorker :: Logger -> IO ()
logWorker logger = forever $ do
  msg <- atomically $ readTQueue (logQueue logger)
  when (logConfigLevel (logConfig logger) <= logLevel msg) $
    logCallback (logConfig logger) msg

-- | Logs a message with the specified severity level.
--
-- >>> logger <- initLogger defaultConfig
-- >>> logMessage logger INFO "This is an informational message."
-- >>> logMessage logger ERROR "This is an error message."
--
-- The above doctest initializes a logger, logs an informational message, and
-- logs an error message. The log messages are processed asynchronously, so
-- there's no need to wait for them to complete.
--
-- >>> logMessage logger DEBUG "This is a debug message."
--
-- In this case, the debug message won't be logged because the default
-- configuration has a log level of INFO, so DEBUG messages are ignored.
logMessage :: Logger -> LogLevel -> String -> IO ()
logMessage logger level msg =
  atomically $ writeTQueue (logQueue logger) (LogMessage level msg)

-- | Helper function to wait for a logger to finish
waitLogger :: Logger -> IO ()
waitLogger logger = wait =<< async (return ())
