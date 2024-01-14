import Test.Tasty
import Test.Tasty.HUnit
import Logger
import Control.Concurrent.STM

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
        logMessage logger DEBUG "This is a debug message."

        logMessages <- atomically $ flushTQueue (logQueue logger)

        assertEqual "Number of log messages" 3 (length logMessages)
        assertBool "INFO log message present" (INFO `elem` map logLevel logMessages)
        assertBool "WARNING log message present" (WARNING `elem` map logLevel logMessages)
        assertBool "ERROR log message present" (ERROR `elem` map logLevel logMessages)
    , testCase "handles custom log levels and callbacks" $ do
        let customConfig = LoggerConfig DEBUG (\msg -> putStrLn $ "Custom Callback: " ++ show msg)
        customLogger <- initLogger customConfig

        logMessage customLogger DEBUG "This is a debug message."
        logMessage customLogger INFO "This is an informational message."

        logMessages <- atomically $ flushTQueue (logQueue customLogger)

        assertEqual "Number of log messages" 2 (length logMessages)
        assertBool "DEBUG log message present" (DEBUG `elem` map logLevel logMessages)
        assertBool "INFO log message present" (INFO `elem` map logLevel logMessages)
    , testCase "logs messages with the correct severity level (Warning)" $ do
        let customConfig = LoggerConfig WARNING (\msg -> putStrLn $ "Custom Callback: " ++ show msg)
        customLogger <- initLogger customConfig

        logMessage customLogger DEBUG "This is a debug message."  -- Won't be logged due to the custom log level
        logMessage customLogger INFO "This is an informational message."  -- Won't be logged due to the custom log level
        logMessage customLogger WARNING "This is a warning message."
        logMessage customLogger ERROR "This is an error message."

        logMessages <- atomically $ flushTQueue (logQueue customLogger)

        assertEqual "Number of log messages" 2 (length logMessages)
        assertBool "WARNING log message present" (WARNING `elem` map logLevel logMessages)
        assertBool "ERROR log message present" (ERROR `elem` map logLevel logMessages)
    ]
