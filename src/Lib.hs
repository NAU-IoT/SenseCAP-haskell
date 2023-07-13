{-# LANGUAGE DeriveDataTypeable #-}

module Lib
  ( someFunc,
    commandEntry,
  )
where

import Control.Monad (forever)
import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)
import System.Serial
import System.Serial.BlockingManager
import System.Serial.SenseCAP
import Repl (runRepl)

data WeatherStation
  = Repl
      { serialPort :: String
      }
  | Query
      { serialPort :: String
      }
  deriving (Data, Typeable, Show, Eq)

repl :: WeatherStation
repl =
  Repl
    { serialPort = "/dev/ttyUSB0"
    }

query :: WeatherStation
query =
  Query
    { serialPort = "/dev/ttyUSB0"
    }

cmdModes :: Mode (CmdArgs WeatherStation)
cmdModes = cmdArgsMode $ modes [repl, query]

commandEntry :: IO ()
commandEntry = do
  myArgs <- getArgs
  opts <- (if null myArgs then withArgs ["--help"] else id) $ cmdArgsRun cmdModes
  argHandler opts

argHandler :: WeatherStation -> IO ()
argHandler w = do
  senseCap <- openSenseCAP (serialPort w) B9600
  manager <- serialManager senseCap 1000
  case w of
    Repl _ -> runRepl manager
    Query _ -> putStrLn "QUERY"

someFunc :: IO ()
someFunc = do
  h <- openSerial "/dev/ttyUSB0" B9600 8 One NoParity NoFlowControl
  manager <- serialManager h 1000
  forever $ do
    putStr "$ "
    line <- getLine
    response <- wrapCommand "\n" ("0XA;" <> line) manager
    case response of
      Just message -> putStrLn message
      Nothing -> putStrLn "Err"
