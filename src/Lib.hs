{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Lib
  ( commandEntry,
  )
where

import Data.Word (Word8)
import Repl (runRepl)
import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)
import System.Hardware.SenseCAP
import System.Hardware.Serialport

data WeatherStation
  = Repl
      { port :: FilePath,
        device :: Word8
      }
  | Query
      { port :: FilePath,
        device :: Word8
      }
  deriving (Data, Typeable, Show, Eq)

portHelp :: String
portHelp = "Serial port to connect to. defaults to /dev/ttyUSB0"

deviceHelp :: String
deviceHelp = "Device ID of the SenseCAP. This is almost always 0 (default)."

repl :: WeatherStation
repl =
  Repl
    { port = "/dev/ttyUSB0" &= typ "PORT" &= help portHelp,
      device = 0 &= typ "DEVICE" &= help deviceHelp
    }

query :: WeatherStation
query =
  Query
    { port = "/dev/ttyUSB0" &= typ "PORT" &= help portHelp,
      device = 0 &= typ "DEVICE" &= help deviceHelp
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
  withSenseCAP (port w) CS9600 $ \cap -> do
    case w of
      Repl _ d -> runRepl cap d
      Query _ _ -> putStrLn "QUERY"
