module System.Hardware.SenseCAP (withSenseCAP, querySenseCAP, getSenseCAP, setSenseCAP, sendCommand) where

import qualified Data.ByteString.Char8 as B
import Data.Word (Word8)
import System.Hardware.Serialport
import System.IO

commandPrefix :: String
commandPrefix = "XA;"

defaultCAPSettings :: CommSpeed -> SerialPortSettings
defaultCAPSettings c =
  SerialPortSettings
    { commSpeed = c,
      bitsPerWord = 8,
      stopb = One,
      parity = NoParity,
      flowControl = Software,
      timeout = 10
    }

withSenseCAP :: FilePath -> CommSpeed -> (SerialPort -> IO a) -> IO a
withSenseCAP port = withSerial port . defaultCAPSettings

-- | Create a query command.
querySenseCAP :: SerialPort -> Word8 -> String -> IO (Maybe String)
querySenseCAP port addr = sendCommand port addr . (<> "=?")

-- | Create a get command.
getSenseCAP :: SerialPort -> Word8 -> String -> IO (Maybe String)
getSenseCAP port addr = sendCommand port addr . (<> "?")

-- | Create a set command.
setSenseCAP :: SerialPort -> Word8 -> String -> String -> IO (Maybe String)
setSenseCAP port addr cmd value= sendCommand port addr $ cmd <> "=" <> value

sendCommand :: SerialPort -> Word8 -> String -> IO (Maybe String)
sendCommand port addr cmd = do
  let addr' = show addr
  let cmd' = addr' <> commandPrefix <> cmd <> "\r\n"
  putStrLn $ "Sending: " <> cmd'
  _ <- send port $ B.pack cmd'
  null' <$> recv port 200
  where
    maybe' a = if null a then Nothing else Just a
    null' = maybe' . B.unpack
