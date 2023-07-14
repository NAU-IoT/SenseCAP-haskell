{-# LANGUAGE ViewPatterns #-}

module Repl(runRepl) where

import System.Hardware.SenseCAP
import GHC.IO.Handle (hFlush)
import System.IO (stdout, Handle)
import Data.List (stripPrefix)
import Data.List.Extra (word1)
import System.Exit (exitSuccess)
import Control.Exception (try)
import Data.Word (Word8)

runRepl :: Handle -> Word8 -> IO ()
runRepl cap device = do
  putStr "$ "
  hFlush stdout
  cmd <- getLine
  res <- (try $ parseCommand cmd cap device) :: IO (Either IOError (Maybe String))
  case res of
    Right (Just r) -> putStr $ "Response: " <> r
    Right Nothing -> putStr "Timed out."
    Left e -> putStr $ "Error: " <> show e
  putStrLn ""
  runRepl cap device

parseCommand :: String -> Handle -> Word8 -> IO (Maybe String)
parseCommand "EXIT" _ _ = exitSuccess
parseCommand (stripPrefix "GET " -> Just arg) p d = getSenseCAP p d arg
parseCommand (stripPrefix "QUERY " -> Just arg) p d = querySenseCAP p d arg
parseCommand (stripPrefix "PUT " -> Just arg) p d = uncurry (setSenseCAP p d) $ word1 arg
parseCommand _ _ _ = return $ Just "Invalid command."
