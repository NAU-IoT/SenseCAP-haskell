module Query (runQuery) where

import Data.Maybe (fromMaybe)
import System.Hardware.SenseCAP

runQuery :: SenseCAP -> IO ()
runQuery cap = do
  res <- getSenseCAP cap "G0"
  putStrLn $ maybe "Error." show res
