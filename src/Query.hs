module Query (runQuery) where

import System.Hardware.SenseCAP

runQuery :: SenseCAP -> IO ()
runQuery cap = do
  res <- getSenseCAP cap "G0"
  putStrLn $ maybe "Error." show res
  name <- getValue cap :: IO (Either String CAPName)
  print name
