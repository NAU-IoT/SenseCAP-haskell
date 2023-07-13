{-# LANGUAGE ViewPatterns #-}
module Repl(runRepl) where

import System.Serial.SenseCAP
import System.Serial.BlockingManager
import GHC.IO.Handle (hFlush)
import System.IO (stdout)
import GHC.IO (catchAny)
import Data.List (stripPrefix)
import Data.List.Extra (word1)

runRepl :: BlockingSerialManager -> IO ()
runRepl cap = do
  putStr "$ "
  hFlush stdout
  cmd <- getLine
  res <- let query = Right <$> parseCommand cmd cap
             ex e = return $ Left $ "IO Error: " <> show e
             in catchAny query ex
  case res of
    Right (Just r) -> putStr $ "Response: " <> r
    Right Nothing -> putStr "Timed out."
    Left e -> putStr e
  putStrLn ""
  runRepl cap

parseCommand :: String -> BlockingSerialManager -> IO (Maybe String)
parseCommand (stripPrefix "GET " -> Just arg) = getSenseCAP arg
parseCommand (stripPrefix "QUERY " -> Just arg) = querySenseCAP arg
parseCommand (stripPrefix "PUT " -> Just arg) = uncurry setSenseCAP $ word1 arg
parseCommand _ = \_ ->  return $ Just "Invalid command."