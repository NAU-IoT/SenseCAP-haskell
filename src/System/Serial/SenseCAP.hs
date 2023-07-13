module System.Serial.SenseCAP (openSenseCAP, querySenseCAP, getSenseCAP, setSenseCAP) where

import System.IO
import System.Serial
import System.Serial.BlockingManager (BlockingSerialManager, wrapCommand)

openSenseCAP :: String -> BaudRate -> IO Handle
openSenseCAP port baud = openSerial port baud 8 One NoParity NoFlowControl

-- | Create a query command.
querySenseCAP :: String -> BlockingSerialManager -> IO (Maybe String)
querySenseCAP param = wrapCommand "\n" $ "0XA;" <> param <> "=?"

-- | Create a get command.
getSenseCAP :: String -> BlockingSerialManager -> IO (Maybe String)
getSenseCAP param = wrapCommand "\n" $ "0XA;" <> param <> "?"

-- | Create a set command.
setSenseCAP :: String -> String -> BlockingSerialManager -> IO (Maybe String)
setSenseCAP param value = wrapCommand "\n" $ "0XA;" <> param <> "=" <> value
