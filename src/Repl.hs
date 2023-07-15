{-# LANGUAGE ViewPatterns #-}

module Repl (runRepl) where

import Control.Exception (try)
import Data.List (stripPrefix)
import Data.List.Extra (word1)
import GHC.IO.Handle (hFlush)
import System.Exit (exitSuccess)
import System.Hardware.SenseCAP
import System.IO (stdout)

helpMenu :: String
helpMenu =
  unlines
    [ "COMMANDS:",
      "  GET   <X>     - Get a value from the sensor (read-only value).",
      "  QUERY <X>     - Get a value from the sensor (read-write value).",
      "  PUT   <X> <Y> - Set a value on the sensor.",
      "  RAW   <CMD>   - Send a raw command to the sensor. Prefix (XA;) and newline are inserted.",
      "  EXIT          - Exit the session. ^C also works.",
      "  HELP          - Display this menu."
    ]

runRepl :: SenseCAP -> IO ()
runRepl cap = do
  putStr "$ "
  hFlush stdout
  cmd <- getLine
  res <- (try $ parseCommand cmd cap) :: IO (Either IOError (Maybe String))
  case res of
    Right (Just r) -> putStr $ "Response: " <> r
    Right Nothing -> putStr "Timed out."
    Left e -> putStr $ "Error: " <> show e
  putStrLn ""
  runRepl cap

unMaybe :: Show a => Functor fa => Functor fb => fa (fb a) -> fa (fb String)
unMaybe = ((show <$>) <$>)

parseCommand :: String -> SenseCAP -> IO (Maybe String)
parseCommand "EXIT" _ = exitSuccess
parseCommand "HELP" _ = return $ Just helpMenu
parseCommand (stripPrefix "GET " -> Just arg) p = unMaybe $ getSenseCAP p arg
parseCommand (stripPrefix "QUERY " -> Just arg) p = unMaybe $ querySenseCAP p arg
parseCommand (stripPrefix "PUT " -> Just arg) p = unMaybe $ uncurry (setSenseCAP p) $ word1 arg
parseCommand (stripPrefix "RAW " -> Just arg) p = unMaybe $ sendCommand p arg
parseCommand _ _ = return $ Just "Invalid command. Type HELP for a list of commands."
