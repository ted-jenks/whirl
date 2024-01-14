module Main where

import Logger
import Control.Concurrent.Async (async, wait)

main :: IO ()
main = do
  logger <- initLogger defaultConfig
  asyncLogger <- async $ do
    logMessage logger INFO "This is an informational message."
    logMessage logger WARNING "This is a warning message."
    logMessage logger ERROR "This is an error message."
    logMessage logger DEBUG "This is a debug message."  -- Won't be logged due to the default log level
    waitLogger logger  -- Use the waitLogger function to wait for the logger to finish
  wait asyncLogger  -- Now, use wait on the Async computation
