{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Hardware.SenseCAP (withSenseCAP, querySenseCAP, getSenseCAP, setSenseCAP, sendCommand, SenseCAP (..)) where

import Data.ByteString.Char8 (hGetLine, hPutStr, pack, unpack)
import Data.Word (Word8)
import System.Console.CmdArgs.Verbosity (whenLoud)
import System.Hardware.Serialport
import System.IO (Handle)

-- | The SenseCAP command prefix, needed for every command.
commandPrefix :: String
commandPrefix = "XA;"

-- | The serial settings to connect to the SenseCAP. The only thing here that can vary is the baud rate.
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

data SenseCAP = SenseCAP
  { -- | The address of the SenseCAP on the serial bus. This is almost always 0 unless there are multiple SenseCAPs on the same bus.
    address :: Word8,
    -- | The baud rate of the serial connection to the SenseCAP.
    baud :: CommSpeed,
    -- | The file handle of the serial port.
    device :: Handle
  }

-- | Perform an IO action with the SenseCAP.
withSenseCAP :: FilePath -> Word8 -> CommSpeed -> (SenseCAP -> IO a) -> IO a
withSenseCAP port addr baud f = hWithSerial port (defaultCAPSettings baud) $ f . SenseCAP addr baud

-- | Create a query command.
querySenseCAP :: SenseCAP -> String -> IO (Maybe String)
querySenseCAP port = sendCommand port . (<> "=?")

-- | Create a get command.
getSenseCAP :: SenseCAP -> String -> IO (Maybe String)
getSenseCAP port = sendCommand port . (<> "?")

-- | Create a set command.
setSenseCAP :: SenseCAP -> String -> String -> IO (Maybe String)
setSenseCAP port cmd value = sendCommand port $ cmd <> "=" <> value

-- | Send a raw command (with the prefix applied) to the SenseCAP.
sendCommand :: SenseCAP -> String -> IO (Maybe String)
sendCommand cap cmd = do
  let port = device cap
  let cmd' = show (address cap) <> commandPrefix <> cmd <> "\r\n"
  whenLoud $ putStrLn $ "Sending: " <> cmd'
  hPutStr port $ pack cmd'
  maybe' . unpack <$> hGetLine port
  where
    maybe' a = if null a then Nothing else Just a
