{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
module Repl(runRepl) where

import System.Hardware.SenseCAP
import GHC.IO.Handle (hFlush)
import System.IO (stdout, Handle)
import GHC.IO (catchAny)
import Data.List (stripPrefix)
import Data.List.Extra (word1)
import System.Exit (exitSuccess)
import Control.Exception.Base (throw, Exception, IOException)
import Control.Exception (try)

runRepl :: Handle -> IO ()
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

parseCommand :: String -> Handle -> IO (Maybe String)
parseCommand "EXIT" _ = exitSuccess
parseCommand (stripPrefix "GET " -> Just arg) p = getSenseCAP p 0 arg
parseCommand (stripPrefix "QUERY " -> Just arg) p = querySenseCAP p 0 arg
parseCommand (stripPrefix "PUT " -> Just arg) p = uncurry (setSenseCAP p 0) $ word1 arg
parseCommand _ _ = return $ Just "Invalid command."
