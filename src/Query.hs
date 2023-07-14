module Query(runQuery) where

import Data.Word (Word8)
import System.Hardware.SenseCAP

import System.IO (Handle)
import Data.Maybe (fromMaybe)



runQuery :: Handle -> Word8 -> IO ()
runQuery cap device = do
  res <- getSenseCAP cap device "G0"
  putStrLn $ fromMaybe "Error." res