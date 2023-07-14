{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Hardware.SenseCAP (withSenseCAP, querySenseCAP, getSenseCAP, setSenseCAP, sendCommand) where

import Data.ByteString.Char8 (hGetLine, hPutStr, pack, unpack)
import Data.Word (Word8)
import System.Hardware.Serialport
import System.IO (Handle)

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

withSenseCAP :: FilePath -> CommSpeed -> (Handle -> IO a) -> IO a
withSenseCAP port = hWithSerial port . defaultCAPSettings

-- | Create a query command.
querySenseCAP :: Handle -> Word8 -> String -> IO (Maybe String)
querySenseCAP port addr = sendCommand port addr . (<> "=?")

-- | Create a get command.
getSenseCAP :: Handle -> Word8 -> String -> IO (Maybe String)
getSenseCAP port addr = sendCommand port addr . (<> "?")

-- | Create a set command.
setSenseCAP :: Handle -> Word8 -> String -> String -> IO (Maybe String)
setSenseCAP port addr cmd value= sendCommand port addr $ cmd <> "=" <> value

sendCommand :: Handle -> Word8 -> String -> IO (Maybe String)
sendCommand port addr cmd = do
  let addr' = show addr
  let cmd' = addr' <> commandPrefix <> cmd <> "\r\n"
  putStrLn $ "Sending: " <> cmd'
  hPutStr port $ pack cmd'
  null' <$> hGetLine port
  where
    maybe' a = if null a then Nothing else Just a
    null' = maybe' . unpack
