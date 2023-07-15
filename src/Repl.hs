{-# LANGUAGE ViewPatterns #-}

module Repl (runRepl) where

import Control.Exception (try)
import Data.List (stripPrefix)
import Data.List.Extra (word1)
import Data.Word (Word8)
import GHC.IO.Handle (hFlush)
import System.Exit (exitSuccess)
import System.Hardware.SenseCAP
import System.IO (Handle, stdout)

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

parseCommand :: String -> SenseCAP -> IO (Maybe String)
parseCommand "EXIT" _ = exitSuccess
parseCommand "HELP" _ = return $ Just helpMenu
parseCommand (stripPrefix "GET " -> Just arg) p = getSenseCAP p arg
parseCommand (stripPrefix "QUERY " -> Just arg) p = querySenseCAP p arg
parseCommand (stripPrefix "PUT " -> Just arg) p = uncurry (setSenseCAP p) $ word1 arg
parseCommand (stripPrefix "RAW " -> Just arg) p = sendCommand p arg
parseCommand _ _ = return $ Just "Invalid command. Type HELP for a list of commands."
