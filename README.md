# Whirl

Whirl is a basic logging library for Haskell. I took on this project to teach
myself the basics of Haskell and experience setting up a full library in the
language.

# Logger API Documentation

The Logger module provides a simple logging functionality with support for different severity levels.

## Types

### LogLevel

Represents the severity levels for log messages.

```haskell
data LogLevel = INFO | WARNING | ERROR | DEBUG deriving (Eq, Ord, Show)
```

### LogMessage

Represents a log message with a severity level and a message.

```haskell
data LogMessage = LogMessage
  { logLevel :: LogLevel  -- ^ Severity level of the log message.
  , message  :: String    -- ^ The log message.
  } deriving (Show)
```

### LoggerConfig

Configuration for the logger.

```haskell
data LoggerConfig = LoggerConfig
  { logConfigLevel :: LogLevel              -- ^ Minimum severity level to log.
  , logCallback    :: LogMessage -> IO ()   -- ^ Callback function to handle log messages.
  }
```

## Functions

### defaultConfig

Returns the default configuration for the logger.

```haskell
defaultConfig :: LoggerConfig
```

### initLogger

Initializes a new logger with the given configuration.

```haskell
initLogger :: LoggerConfig -> IO Logger
```

### logMessage

Logs a message with the specified severity level.

```haskell
logMessage :: Logger -> LogLevel -> String -> IO ()
```

### waitLogger

Helper function to wait for a logger to finish.

```haskell
waitLogger :: Logger -> IO ()
```

## Example Usage

```haskell
Copy code
-- Initialize logger with default configuration
logger <- initLogger defaultConfig

-- Log messages with different severity levels
logMessage logger INFO "This is an informational message."
logMessage logger WARNING "This is a warning message."
logMessage logger ERROR "This is an error message."
logMessage logger DEBUG "This is a debug message."

-- Wait for the logger to finish
waitLogger logger
```

This example demonstrates how to use the Logger API to initialize a logger, log messages with various severity levels, and wait for the logger to finish processing.
