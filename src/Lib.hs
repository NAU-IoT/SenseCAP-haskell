{-# LANGUAGE DeriveDataTypeable #-}

module Lib
  ( commandEntry,
  )
where

import Repl (runRepl)
import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)
import System.Hardware.SenseCAP
import System.Hardware.Serialport

data WeatherStation
  = Repl
      { serialPort :: FilePath
      }
  | Query
      { serialPort :: FilePath
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
  withSenseCAP (serialPort w) CS9600 $ \cap -> do
    case w of
      Repl _ -> runRepl cap
      Query _ -> putStrLn "QUERY"
