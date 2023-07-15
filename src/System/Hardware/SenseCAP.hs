{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Hardware.SenseCAP
  ( withSenseCAP,
    querySenseCAP,
    getSenseCAP,
    setSenseCAP,
    sendCommand,
    SenseCAP (..),
    SenseCAPResponse (..),
    valueName,
  )
where

import Control.Monad ((<=<))
import Data.ByteString.Char8 (hGetLine, hPutStr, pack, unpack)
import Data.Function ((&))
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import System.Console.CmdArgs.Verbosity (whenLoud)
import System.Hardware.Serialport
import System.IO (Handle)
import Text.Read (readMaybe)

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
querySenseCAP :: SenseCAP -> String -> IO (Maybe [SenseCAPResponse])
querySenseCAP port = sendCommand port . (<> "=?")

-- | Create a get command.
getSenseCAP :: SenseCAP -> String -> IO (Maybe [SenseCAPResponse])
getSenseCAP port = sendCommand port . (<> "?")

-- | Create a set command.
setSenseCAP :: SenseCAP -> String -> String -> IO (Maybe [SenseCAPResponse])
setSenseCAP port cmd value = sendCommand port $ cmd <> "=" <> value

-- | Send a raw command (with the prefix applied) to the SenseCAP.
-- | Note that this function also ensures a properly formatted response, and will return 'Nothing' otherwise.
-- | If debugging, consider directly interacting with the 'Handle'.
sendCommand :: SenseCAP -> String -> IO (Maybe [SenseCAPResponse])
sendCommand cap cmd = do
  let port = device cap
      pre = show (address cap) <> commandPrefix
      cmd' = pre <> cmd <> "\r\n"
  whenLoud $ putStrLn $ "Sending: " <> cmd'
  hPutStr port $ pack cmd'
  (parseResponse <=< stripPrefix pre . unpack) <$> hGetLine port

-- | Get the name of a response value
valueName :: SenseCAPResponse -> String
valueName (IntResponse _ s) = s
valueName (DoubleResponse _ s) = s
valueName (TextResponse _ s) = s

-- | A response (key-value) from the SenseCAP.
data SenseCAPResponse
  = -- | Response containing an Int
    IntResponse Int String
  | -- | Response containing a Double
    DoubleResponse Double String
  | -- | Response containing a String
    TextResponse String String
  deriving (Show, Eq)

-- | Split a list on a value.
split :: Eq a => [a] -> a -> [[a]]
split input splitter = uncurry (:) $ foldr squash (mempty, mempty) input
  where
    squash test (run, res) = if test == splitter then (mempty, run : res) else (test : run, res)

-- | Parse a semicolon-delimited set of responses.
parseResponse :: String -> Maybe [SenseCAPResponse]
parseResponse s = mapM parseSingle $ split s ';'

-- | Parse a single key-value pair into a 'SenseCAPResponse'
parseSingle :: String -> Maybe SenseCAPResponse
parseSingle s = assemble $ split s '='
  where
    assemble :: [String] -> Maybe SenseCAPResponse
    assemble [fieldName, fieldValue] =
      Just $
        fieldName
          & let maybeInt = IntResponse <$> readMaybe fieldValue
                maybeDouble = DoubleResponse <$> readMaybe fieldValue
                text = TextResponse fieldValue
             in fromMaybe text $ firstJust maybeInt maybeDouble
    assemble _ = Nothing
    firstJust (Just a) _ = Just a
    firstJust Nothing (Just a) = Just a
    firstJust _ _ = Nothing
