module Query (runQuery) where

import System.Hardware.SenseCAP
import Data.Yaml.Aeson
import System.Hardware.Serialport
import Config
import Data.ByteString.Char8 (unpack)

runQuery :: SenseCAP -> IO ()
runQuery cap = do
  res <- getSenseCAP cap "G0"
  putStrLn $ maybe "Error." show res
  name <- getValue cap :: IO (Either String CAPName)
  print name
  tempSet <- setValue cap $ CAPTemperatureUnit Celsius
  print tempSet
  temp <- getValue cap :: IO (Either String CAPAirTemperature)
  print temp
  serial <- getValue cap :: IO (Either String CAPSerial)
  print serial
  comms <- readConfig cap
  putStrLn $ unpack $ encode comms
