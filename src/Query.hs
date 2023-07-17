module Query (runQuery) where

import System.Hardware.SenseCAP
import Data.Aeson.Types (toJSON)
import System.Hardware.Serialport

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
  tilt <- getValue cap :: IO (Either String CAPFallDetection)
  print tilt
  print $ toJSON $ CAPBaudRate BD9600
