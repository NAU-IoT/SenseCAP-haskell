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
    SenseCAPRead,
    getValue,
    setValue,
    SenseCAPWrite,
    CAPName,
    CAPBaudRate
  )
where

import Control.Monad ((<=<))
import Data.ByteString.Char8 (hGetLine, hPutStr, pack, unpack)
import Data.Either.Extra (maybeToEither)
import Data.Foldable (find)
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

-- | Get the name of a response value
valueAsString :: SenseCAPResponse -> String
valueAsString (IntResponse i _) = show i
valueAsString (DoubleResponse d _) = show d
valueAsString (TextResponse s _) = s

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
split :: Eq a => a -> [a] -> [[a]]
split splitter = uncurry (:) . foldr squash (mempty, mempty)
  where
    squash test (run, res) = if test == splitter then (mempty, run : res) else (test : run, res)

-- | Parse a semicolon-delimited set of responses.
parseResponse :: String -> Maybe [SenseCAPResponse]
parseResponse = mapM parseSingle . split ';' . filter (/= '\r')

-- | Parse a single key-value pair into a 'SenseCAPResponse'
parseSingle :: String -> Maybe SenseCAPResponse
parseSingle = assemble . split '='
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

findValue :: String -> [SenseCAPResponse] -> Either String SenseCAPResponse
findValue n = maybeToEither ("Response does not contain required value: " <> n) . find ((== n) . valueName)

response :: Maybe [SenseCAPResponse] -> Either String [SenseCAPResponse]
response = maybeToEither "No response from SenseCAP."

class (Show a) => SenseCAPRead a where
  getValue :: SenseCAP -> IO (Either String a)

  parseValue :: SenseCAPResponse -> Either String a

class (Show a) => SenseCAPWrite a where
  setValue :: SenseCAP -> a -> IO (Either String a)

  unParseValue :: a -> String
  unParseValue = show

newtype SenseCAPValue a = SenseCAPValue {value :: a}
  deriving (Show)

type CAPBaudRate = SenseCAPValue CommSpeed

instance SenseCAPRead CAPBaudRate where
  getValue cap = findB <$> querySenseCAP cap "BD"
    where
      findB :: Maybe [SenseCAPResponse] -> Either String CAPBaudRate
      findB i = do
        res <- response i
        b <- findValue "BD" res
        parseValue b
  parseValue a = SenseCAPValue <$> (toBaud a >>= int)
    where
      toBaud :: SenseCAPResponse -> Either String Int
      toBaud (IntResponse i _) = Right i
      toBaud i = Left $ "Response given was not an integer: " <> show i
      int :: Int -> Either String CommSpeed
      int 96 = Right CS9600
      int 192 = Right CS19200
      int 384 = Right CS38400
      int 576 = Right CS57600
      int 1152 = Right CS115200
      int i = Left $ "Invalid baudrate received: " <> show i

type CAPName = SenseCAPValue String

instance SenseCAPRead CAPName where
  getValue cap = (\i -> response i >>= findValue "NA" >>= parseValue) <$> querySenseCAP cap "NA"

  parseValue = Right . SenseCAPValue . valueAsString
