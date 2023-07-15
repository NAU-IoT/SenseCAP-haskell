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
        baud_ :: CommSpeed
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
    }

query :: WeatherStation
query =
  Query
    { port = "/dev/ttyUSB0" &= typ "PORT" &= help portHelp,
      device_ = 0 &= typ "DEVICE" &= help deviceHelp,
      baud_ = CS9600 &= help baudHelp
    }

cmdModes :: Mode (CmdArgs WeatherStation)
cmdModes = cmdArgsMode $ modes [repl, query]

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
