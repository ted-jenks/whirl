import Test.Tasty
import Test.Tasty.HUnit
import Logger
import Control.Concurrent.Async (async, wait)

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Tests" [loggerTests]

loggerTests :: TestTree
loggerTests =
  testGroup
    "Logger"
    [ testCase "logs messages with the correct severity level" $ do
        logger <- initLogger defaultConfig
        logMessage logger INFO "This is an informational message."
        logMessage logger WARNING "This is a warning message."
        logMessage logger ERROR "This is an error message."
        logMessage logger DEBUG "This is a debug message."  -- Won't be logged due to the default log level
        asyncLogger <- async $ waitLogger logger
        wait asyncLogger
    , testCase "handles custom log levels and callbacks" $ do
        let customConfig = LoggerConfig DEBUG (\msg -> putStrLn $ "Custom Callback: " ++ show msg)
        customLogger <- initLogger customConfig
        logMessage customLogger DEBUG "This is a debug message."
        logMessage customLogger INFO "This is an informational message."
        asyncCustomLogger <- async $ waitLogger customLogger
        wait asyncCustomLogger
    ]
