{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-cse #-}

module Lib
  ( commandEntry,
  )
where

import Data.Word (Word8)
import Repl (runRepl)
import Query (runQuery)
import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)
import System.Hardware.SenseCAP
import System.Hardware.Serialport

deriving instance Data CommSpeed

data WeatherStation
  = Repl
      { port :: FilePath,
        device_ :: Word8,
        baud_ :: CommSpeed
      }
  | Query
      { port :: FilePath,
        device_ :: Word8,
        baud_ :: CommSpeed,
        config :: FilePath
      }
  deriving (Data, Typeable, Show, Eq)

portHelp :: String
portHelp = "Serial port to connect to. defaults to /dev/ttyUSB0."

deviceHelp :: String
deviceHelp = "Device ID of the SenseCAP. This is almost always 0 (default)."

baudHelp :: String
baudHelp = "Baudrate of serial connection (9600 default)."

repl :: WeatherStation
repl =
  Repl
    { port = "/dev/ttyUSB0" &= typ "PORT" &= help portHelp,
      device_ = 0 &= typ "DEVICE" &= help deviceHelp,
      baud_ = CS9600 &= help baudHelp
    } &=  help "Launch a REPL to issue commands to and interact with the sensor. Useful for debugging the sensor."

query :: WeatherStation
query =
  Query
    { port = "/dev/ttyUSB0" &= typ "PORT" &= help portHelp,
      device_ = 0 &= typ "DEVICE" &= help deviceHelp,
      baud_ = CS9600 &= help baudHelp,
      config = "config.yml" &= help "Config file to use. Defaults to config.yml."
    } &= help "Query values from the sensor using a config file."

cmdModes :: Mode (CmdArgs WeatherStation)
cmdModes = cmdArgsMode $ modes [repl, query] &= verbosity

commandEntry :: IO ()
commandEntry = do
  myArgs <- getArgs
  opts <- (if null myArgs then withArgs ["--help"] else id) $ cmdArgsRun cmdModes
  argHandler opts

argHandler :: WeatherStation -> IO ()
argHandler w = withSenseCAP (port w) (device_ w) (baud_ w) $ \cap -> do
  case w of
    Repl {} -> runRepl cap
    Query {} -> runQuery cap

