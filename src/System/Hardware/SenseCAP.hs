{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}

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
    CAPBaudRate,
  )
where

import Control.Monad ((<=<))
import Data.ByteString.Char8 (hGetLine, hPutStr, pack, unpack)
import Data.Coerce (coerce, Coercible)
import Data.Either.Extra (maybeToEither)
import Data.Foldable (find)
import Data.Function ((&))
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import Language.Haskell.TH
import System.Console.CmdArgs.Verbosity (whenLoud)
import System.Hardware.ParameterTH
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

data CommProtocol = SDI12 | Modbus | ASCII deriving (Show, Eq)

data CompassState = CompassEnable | CompassDisable | CompassGeomagnetic deriving (Show, Eq)

data RainResetMode = Manual | PostRead | Overflow deriving (Show, Eq)

data TemperatureUnit = Celsius | Farenheit deriving (Show, Eq)

data PressureUnit = Pascal | HectoPascal | Bar | MMHg | InHg deriving (Show, Eq)

data SpeedUnit = MetersPerSecond | KilometersPerHour | MilesPerHour | Knots deriving (Show, Eq)

data LengthUnit = Millimeters | Inches deriving (Show, Eq)

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

valueAsString :: SenseCAPResponse -> Either String String
valueAsString (IntResponse i _) = Right $ show i
valueAsString (DoubleResponse d _) = Right $ show d
valueAsString (TextResponse s _) = Right s

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

extract :: SenseCAPRead b => String -> Maybe [SenseCAPResponse] -> Either String b
extract n i = response i >>= findValue n >>= parseValue

parseCommSpeed :: Int -> Either String CommSpeed
parseCommSpeed 96 = Right CS9600
parseCommSpeed 192 = Right CS19200
parseCommSpeed 384 = Right CS38400
parseCommSpeed 576 = Right CS57600
parseCommSpeed 1152 = Right CS115200
parseCommSpeed i = Left $ "Invalid baudrate received: " <> show i

unParseCommSpeed :: CommSpeed -> String
unParseCommSpeed CS9600 = "96"
unParseCommSpeed CS19200 = "192"
unParseCommSpeed CS38400 = "384"
unParseCommSpeed CS57600 = "576"
unParseCommSpeed CS115200 = "1152"
unParseCommSpeed _ = "96" -- default

toBaud :: SenseCAPResponse -> Either String CommSpeed
toBaud a = toBaud' a >>= parseCommSpeed
  where
    toBaud' :: SenseCAPResponse -> Either String Int
    toBaud' (IntResponse i _) = Right i
    toBaud' i = Left $ "Response given was not an integer: " <> show i



class (Show a) => SenseCAPRead a where
  getValue :: SenseCAP -> IO (Either String a)

  parseValue :: SenseCAPResponse -> Either String a

class (Show a) => SenseCAPWrite a where
  setValue :: SenseCAP -> a -> IO (Either String a)

  unParseValue :: a -> String
  unParseValue = show

-- $(instanceRead "UT" "CAPModel")

-- device parameters

newtype CAPAddress = CAPAddress Word8 deriving (Show, Eq)

newtype CAPBaudRate = CAPBaudRate CommSpeed deriving (Show, Eq)
$(instanceRead "BD" "CAPBaudRate" "toBaud" CAPQuery)
$(instanceWrite "BD" "CAPBaudRate" $ Just "unParseCommSpeed")

newtype CAPProtocol = CAPProtocol CommProtocol deriving (Show, Eq)

newtype CAPModbusAddress = CAPModbusAddress Int deriving (Show, Eq)

newtype CAPRS485BaudRate = CAPRS485BaudRate CommSpeed deriving (Show, Eq)
$(instanceRead "MBBD" "CAPRS485BaudRate" "toBaud" CAPQuery)
$(instanceWrite "MBBD" "CAPRS485BaudRate" $ Just "unParseCommSpeed")

newtype CAPName = CAPName String deriving (Show, Eq)
$(instanceRead "NA" "CAPName" "valueAsString" CAPQuery)
$(instanceWrite "NA" "CAPName" Nothing)

newtype CAPModel = CAPModel String deriving (Show, Eq)
$(instanceRead "TP" "CAPModel" "valueAsString" CAPQuery)

newtype CAPVersion = CAPVersion String deriving (Show, Eq)
$(instanceRead "VE" "CAPVersion" "valueAsString" CAPQuery)

newtype CAPSerial = CAPSerial Int deriving (Show, Eq)

newtype CAPProductionDate = CAPProductionDate String deriving (Show, Eq)
$(instanceRead "MD" "CAPProductionDate" "valueAsString" CAPQuery)

newtype CAPRestoreConfig = CAPRestoreConfig Bool deriving (Show, Eq)

newtype CAPCompassState = CAPCompassState CompassState deriving (Show, Eq)

newtype CAPTiltDetect = CAPTiltDetect Bool deriving (Show, Eq)

newtype CAPHeating = CAPHeating Bool deriving (Show, Eq)

-- sensor values

newtype CAPAirTemperature = CAPAirTemperature Double deriving (Show, Eq)

newtype CAPAirHumidity = CAPAirHumidity Double deriving (Show, Eq)

newtype CAPBarometricPressure = CAPBarometricPressure Double deriving (Show, Eq)

newtype CAPLightIntensity = CAPLightIntensity Double deriving (Show, Eq)

newtype CAPMinimumWindDirection = CAPMinimumWindDirection Double deriving (Show, Eq)

newtype CAPMaximumWindDirection = CAPMaximumWindDirection Double deriving (Show, Eq)

newtype CAPAverageWindDirection = CAPAverageWindDirection Double deriving (Show, Eq)

newtype CAPMinimumWindSpeed = CAPMinimumWindSpeed Double deriving (Show, Eq)

newtype CAPMaximumWindSpeed = CAPMaximumWindSpeed Double deriving (Show, Eq)

newtype CAPAverageWindSpeed = CAPAverageWindSpeed Double deriving (Show, Eq)

newtype CAPAccumulatedRainfall = CAPAccumulatedRainfall Double deriving (Show, Eq)

newtype CAPRainfallDuration = CAPRainfallDuration Double deriving (Show, Eq)

newtype CAPRainfallIntensity = CAPRainfallIntensity Double deriving (Show, Eq)

newtype CAPMaximumRainfallIntensity = CAPMaximumRainfallIntensity Double deriving (Show, Eq)

newtype CAPHeatingTemperature = CAPHeatingTemperature Double deriving (Show, Eq)

newtype CAPFallDetection = CAPFallDetection Bool deriving (Show, Eq)

-- sensor units/update intervals

newtype CAPTemperatureUpdateInterval = CAPTemperatureUpdateInterval Int deriving (Show, Eq)

newtype CAPTemperatureUnit = CAPTemperatureUnit TemperatureUnit deriving (Show, Eq)

newtype CAPPressureUnit = CAPPressureUnit PressureUnit deriving (Show, Eq)

newtype CAPWindUpdateInterval = CAPWindUpdateInterval Int deriving (Show, Eq)

newtype CAPWindTimeWindow = CAPWindTimeWindow Int deriving (Show, Eq)

newtype CAPWindSpeedUnit = CAPWindSpeedUnit SpeedUnit deriving (Show, Eq)

newtype CAPWindOffsetCorrection = CAPWindOffsetCorrection Int deriving (Show, Eq)

newtype CAPRainUpdateInterval = CAPRainUpdateInterval Int deriving (Show, Eq)

newtype CAPRainUnit = CAPRainUnit LengthUnit deriving (Show, Eq)

newtype CAPRainResetMode = CAPRainResetMode RainResetMode deriving (Show, Eq)

newtype CAPRainOverflowValue = CAPRainOverflowValue Int deriving (Show, Eq)

newtype CAPRainDurationOverflowValue = CAPRainDurationOverflowValue Int deriving (Show, Eq)

newtype CAPClearRain = CAPClearRain Bool deriving (Show, Eq)

newtype CAPClearRainDuration = CAPClearRainDuration Bool deriving (Show, Eq)

