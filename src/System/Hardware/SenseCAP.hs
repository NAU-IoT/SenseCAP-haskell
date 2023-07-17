{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
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
    CAPName (..),
    CAPBaudRate (..),
    CAPModel (..),
    CAPAccumulatedRainfall (..),
    CAPAirHumidity (..),
    CAPAirTemperature (..),
    CAPAverageWindDirection (..),
    CAPAverageWindSpeed (..),
    CAPBarometricPressure (..),
    CAPClearRain (..),
    CAPClearRainDuration (..),
    CAPCompassState (..),
    CAPFallDetection (..),
    CAPHeating (..),
    CAPHeatingTemperature (..),
    CAPLightIntensity (..),
    CAPMaximumRainfallIntensity (..),
    CAPMaximumWindDirection (..),
    CAPMaximumWindSpeed (..),
    CAPMinimumWindDirection (..),
    CAPMinimumWindSpeed (..),
    CAPModbusAddress (..),
    CAPRainDurationOverflowValue (..),
    CAPRainOverflowValue (..),
    CAPPressureUnit (..),
    CAPRainUnit (..),
    CAPTemperatureUnit (..),
    CAPProtocol (..),
    CAPWindSpeedUnit (..),
    CAPRS485BaudRate (..),
    CAPVersion (..),
    CAPSerial (..),
    CAPProductionDate (..),
    CAPTiltDetect (..),
    CAPRainfallDuration (..),
    CAPRainfallIntensity (..),
    CAPRainResetMode (..),
    CAPRestoreConfig (..),
    CAPRainUpdateInterval (..),
    CAPTemperatureUpdateInterval (..),
    CAPWindUpdateInterval (..),
    CAPWindTimeWindow (..),
    CAPWindOffsetCorrection (..),
    TemperatureUnit (..),
    LengthUnit (..),
    SpeedUnit (..),
    PressureUnit (..),
    BaudRate (..),
  )
where

import Control.Monad ((<=<))
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.ByteString.Char8 (hGetLine, hPutStr, pack, unpack)
import Data.Coerce (coerce)
import Data.Either.Extra (maybeToEither)
import Data.Foldable (find)
import Data.Function ((&))
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import GHC.Generics (Generic)
import System.Console.CmdArgs.Verbosity (whenLoud)
import System.Hardware.TH
import System.Hardware.Serialport
import System.IO (Handle)
import Text.Read (readMaybe)

-- | The communication protocol the SenseCAP is using. Unless you are accessing the SenseCAP over the USB interface this will obviously be 'ASCII'
data CommProtocol = SDI12 | Modbus | ASCII deriving (Show, Eq, Generic)

$(instancesJSON "CommProtocol")

data CompassState = CompassEnable | CompassDisable | CompassGeomagnetic deriving (Show, Eq, Generic)

$(instancesJSON "CompassState")

data RainResetMode = Manual | PostRead | Overflow deriving (Show, Eq, Generic)

$(instancesJSON "RainResetMode")

-- | Temperature unit the SenseCAP uses. Either 'Farenheit' or 'Celsius'.
data TemperatureUnit = Celsius | Farenheit deriving (Show, Eq, Generic)

$(instancesJSON "TemperatureUnit")

-- | Pressure unit the SenseCAP uses for atmospheric pressure.
data PressureUnit = Pascal | HectoPascal | Bar | MMHg | InHg deriving (Show, Eq, Generic)

$(instancesJSON "PressureUnit")

-- | Speed unit the SenseCAP uses for wind speed.
data SpeedUnit = MetersPerSecond | KilometersPerHour | MilesPerHour | Knots deriving (Show, Eq, Generic)

$(instancesJSON "SpeedUnit")

-- | Length unit the SenseCap uses for rain accumulation.
data LengthUnit = Millimeters | Inches deriving (Show, Eq, Generic)

$(instancesJSON "LengthUnit")

data BaudRate = BD9600 | BD19200 | BD38400 | BD57600 | BD115200 deriving (Show, Eq, Generic)

$(instancesJSON "BaudRate")

data SenseCAP = SenseCAP
  { -- | The address of the SenseCAP on the serial bus. This is almost always 0 unless there are multiple SenseCAPs on the same bus.
    address :: Word8,
    -- | The baud rate of the serial connection to the SenseCAP.
    baud :: BaudRate,
    -- | The file handle of the serial port.
    device :: Handle
  }

-- | The SenseCAP command prefix, needed for every command.
commandPrefix :: String
commandPrefix = "XA;"

-- | The serial settings to connect to the SenseCAP. The only thing here that can vary is the baud rate.
defaultCAPSettings :: BaudRate -> SerialPortSettings
defaultCAPSettings c =
  SerialPortSettings
    { commSpeed = toCommSpeed c,
      bitsPerWord = 8,
      stopb = One,
      parity = NoParity,
      flowControl = Software,
      timeout = 10
    }

toCommSpeed :: BaudRate -> CommSpeed
toCommSpeed b = case b of
  BD9600 -> CS9600
  BD19200 -> CS19200
  BD38400 -> CS38400
  BD57600 -> CS57600
  BD115200 -> CS115200

-- | Perform an IO action with the SenseCAP.
withSenseCAP :: FilePath -> Word8 -> BaudRate -> (SenseCAP -> IO a) -> IO a
withSenseCAP port addr baud' f = hWithSerial port (defaultCAPSettings baud') $ f . SenseCAP addr baud'

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
  deriving (Show, Eq, Generic)

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

fromBaud :: BaudRate -> String
fromBaud BD9600 = "96"
fromBaud BD19200 = "192"
fromBaud BD38400 = "384"
fromBaud BD57600 = "576"
fromBaud BD115200 = "1152"

toBaud :: SenseCAPResponse -> Either String BaudRate
toBaud a = toBaud' a >>= parseCommSpeed
  where
    toBaud' :: SenseCAPResponse -> Either String Int
    toBaud' (IntResponse i _) = Right i
    toBaud' i = Left $ "Response given was not an integer: " <> show i
    parseCommSpeed :: Int -> Either String BaudRate
    parseCommSpeed 96 = Right BD9600
    parseCommSpeed 192 = Right BD19200
    parseCommSpeed 384 = Right BD38400
    parseCommSpeed 576 = Right BD57600
    parseCommSpeed 1152 = Right BD115200
    parseCommSpeed i = Left $ "Invalid baudrate received: " <> show i

toDouble :: SenseCAPResponse -> Either String Double
toDouble (DoubleResponse d _) = Right d
toDouble (IntResponse d _) = Right $ fromIntegral d
toDouble r = Left $ "Response was not a double: " <> show r

toInt :: SenseCAPResponse -> Either String Int
toInt (IntResponse i _) = Right i
toInt r = Left $ "Response was not an integer: " <> show r

fromInt :: Int -> String
fromInt = show

toTemperatureUnit :: SenseCAPResponse -> Either String TemperatureUnit
toTemperatureUnit (TextResponse s _) = case s of
  "C" -> Right Celsius
  "F" -> Right Farenheit
  _ -> Left $ "Invalid temperature unit received: " <> s
toTemperatureUnit s = Left $ "Invalid data type for temperature unit: " <> show s

fromTemperatureUnit :: TemperatureUnit -> String
fromTemperatureUnit Farenheit = "F"
fromTemperatureUnit Celsius = "C"

toPressureUnit :: SenseCAPResponse -> Either String PressureUnit
toPressureUnit (TextResponse s _) = case s of
  "P" -> Right Pascal
  "H" -> Right HectoPascal
  "B" -> Right Bar
  "M" -> Right MMHg
  "I" -> Right InHg
  _ -> Left $ "Invalid pressure unit received: " <> s
toPressureUnit s = Left $ "Invalid data type for pressure unit: " <> show s

fromPressureUnit :: PressureUnit -> String
fromPressureUnit Pascal = "P"
fromPressureUnit HectoPascal = "H"
fromPressureUnit Bar = "B"
fromPressureUnit MMHg = "M"
fromPressureUnit InHg = "I"

toLengthUnit :: SenseCAPResponse -> Either String LengthUnit
toLengthUnit (TextResponse s _) = case s of
  "M" -> Right Millimeters
  "I" -> Right Inches
  _ -> Left $ "Invalid length unit received: " <> s
toLengthUnit s = Left $ "Invalid data type for length unit: " <> show s

fromLengthUnit :: LengthUnit -> String
fromLengthUnit Millimeters = "M"
fromLengthUnit Inches = "I"

toSpeedUnit :: SenseCAPResponse -> Either String SpeedUnit
toSpeedUnit (TextResponse s _) = case s of
  "M" -> Right MetersPerSecond
  "K" -> Right KilometersPerHour
  "S" -> Right MilesPerHour
  "N" -> Right Knots
  _ -> Left $ "Invalid speed unit received: " <> s
toSpeedUnit s = Left $ "Invalid data type for speed unit: " <> show s

fromSpeedUnit :: SpeedUnit -> String
fromSpeedUnit MetersPerSecond = "M"
fromSpeedUnit KilometersPerHour = "K"
fromSpeedUnit MilesPerHour = "S"
fromSpeedUnit Knots = "N"

toRainResetMode :: SenseCAPResponse -> Either String RainResetMode
toRainResetMode (TextResponse s _) = case s of
  "M" -> Right Manual
  "A" -> Right PostRead
  "L" -> Right Overflow
  _ -> Left $ "Invalid rain reset mode received: " <> s
toRainResetMode s = Left $ "Invalid data type for rain reset mode: " <> show s

fromRainResetMode :: RainResetMode -> String
fromRainResetMode Manual = "M"
fromRainResetMode PostRead = "A"
fromRainResetMode Overflow = "L"

toCompassState :: SenseCAPResponse -> Either String CompassState
toCompassState (TextResponse s _) = case s of
  "Y" -> Right CompassEnable
  "N" -> Right CompassDisable
  "C" -> Right CompassGeomagnetic
  _ -> Left $ "Invalid compass state received: " <> s
toCompassState s = Left $ "Invalid data type for compass state: " <> show s

fromCompassState :: CompassState -> String
fromCompassState CompassEnable = "Y"
fromCompassState CompassDisable = "N"
fromCompassState CompassGeomagnetic = "C"

toCommProtocol :: SenseCAPResponse -> Either String CommProtocol
toCommProtocol (IntResponse i _) = case i of
  1 -> Right SDI12
  2 -> Right Modbus
  3 -> Right ASCII
  _ -> Left $ "Got invalid communication protocol ID: " <> show i
toCommProtocol s = Left $ "Invalid data type for communication protocol: " <> show s

fromCommProtocol :: CommProtocol -> String
fromCommProtocol SDI12 = "1"
fromCommProtocol Modbus = "2"
fromCommProtocol ASCII = "3"

toBool :: SenseCAPResponse -> Either String Bool
toBool (IntResponse i _) = case i of
  0 -> Right False
  1 -> Right True
  _ -> Left $ "Got invalid value for boolean parameter: " <> show i
toBool s = Left $ "Invalid data type for boolean parameter: " <> show s

fromBool :: Bool -> String
fromBool False = "0"
fromBool True = "1"

toBoolText :: SenseCAPResponse -> Either String Bool
toBoolText (TextResponse i _) = case i of
  "N" -> Right False
  "Y" -> Right True
  _ -> Left $ "Got invalid value for boolean parameter: " <> show i
toBoolText s = Left $ "Invalid data type for boolean parameter: " <> show s

fromBoolText :: Bool -> String
fromBoolText False = "N"
fromBoolText True = "Y"

-- | Encapsulates a value which can be read from the SenseCAP.
class (Show a) => SenseCAPRead a where
  getValue :: SenseCAP -> IO (Either String a)

  parseValue :: SenseCAPResponse -> Either String a

-- | Encapsulates a value which can be written to the SenseCAP.
class (Show a) => SenseCAPWrite a where
  setValue :: SenseCAP -> a -> IO (Either String a)

  unParseValue :: a -> String
  unParseValue = show

-- device parameters

newtype CAPBaudRate = CAPBaudRate BaudRate deriving (Show, Eq, Generic)

$(instancesReadAndJSON "BD" "CAPBaudRate" "toBaud" CAPQuery)
$(instanceWrite "BD" "CAPBaudRate" $ Just "fromBaud")

newtype CAPProtocol = CAPProtocol CommProtocol deriving (Show, Eq, Generic)

$(instancesReadAndJSON "CP" "CAPProtocol" "toCommProtocol" CAPQuery)
$(instanceWrite "CP" "CAPProtocol" $ Just "fromCommProtocol")

newtype CAPModbusAddress = CAPModbusAddress Int deriving (Show, Eq, Generic)

$(instancesReadAndJSON "MBAD" "CAPModbusAddress" "toInt" CAPQuery)
$(instanceWrite "MBAD" "CAPModbusAddress" $ Just "fromInt")

newtype CAPRS485BaudRate = CAPRS485BaudRate BaudRate deriving (Show, Eq, Generic)

$(instancesReadAndJSON "MBBD" "CAPRS485BaudRate" "toBaud" CAPQuery)
$(instanceWrite "MBBD" "CAPRS485BaudRate" $ Just "fromBaud")

newtype CAPName = CAPName String deriving (Show, Eq, Generic)

$(instancesReadAndJSON "NA" "CAPName" "valueAsString" CAPQuery)
$(instanceWrite "NA" "CAPName" Nothing)

newtype CAPModel = CAPModel String deriving (Show, Eq, Generic)

$(instancesReadAndJSON "TP" "CAPModel" "valueAsString" CAPQuery)

newtype CAPVersion = CAPVersion String deriving (Show, Eq, Generic)

$(instancesReadAndJSON "VE" "CAPVersion" "valueAsString" CAPQuery)

newtype CAPSerial = CAPSerial Int deriving (Show, Eq, Generic)

$(instancesReadAndJSON "S/N" "CAPSerial" "toInt" CAPQuery)

newtype CAPProductionDate = CAPProductionDate String deriving (Show, Eq, Generic)

$(instancesReadAndJSON "MD" "CAPProductionDate" "valueAsString" CAPQuery)

newtype CAPRestoreConfig = CAPRestoreConfig Bool deriving (Show, Eq, Generic)

$(instancesReadAndJSON "RESTORE" "CAPRestoreConfig" "toBool" CAPQuery)
$(instanceWrite "RESTORE" "CAPRestoreConfig" $ Just "fromBool")

newtype CAPCompassState = CAPCompassState CompassState deriving (Show, Eq, Generic)

$(instancesReadAndJSON "CC" "CAPCompassState" "toCompassState" CAPQuery)
$(instanceWrite "CC" "CAPCompassState" $ Just "fromCompassState")

newtype CAPTiltDetect = CAPTiltDetect Bool deriving (Show, Eq, Generic)

$(instancesReadAndJSON "TD" "CAPTiltDetect" "toBoolText" CAPQuery)
$(instanceWrite "TD" "CAPTiltDetect" $ Just "fromBoolText")

newtype CAPHeating = CAPHeating Bool deriving (Show, Eq, Generic)

$(instancesReadAndJSON "HC" "CAPHeating" "toBoolText" CAPQuery)
$(instanceWrite "HC" "CAPHeating" $ Just "fromBoolText")

-- sensor values

newtype CAPAirTemperature = CAPAirTemperature Double deriving (Show, Eq, Generic)

$(instancesReadAndJSON' "G0" "AT" "CAPAirTemperature" "toDouble" CAPGet)

newtype CAPAirHumidity = CAPAirHumidity Double deriving (Show, Eq, Generic)

$(instancesReadAndJSON' "G0" "AH" "CAPAirHumidity" "toDouble" CAPGet)

newtype CAPBarometricPressure = CAPBarometricPressure Int deriving (Show, Eq, Generic)

$(instancesReadAndJSON' "G0" "AP" "CAPBarometricPressure" "toInt" CAPGet)

newtype CAPLightIntensity = CAPLightIntensity Int deriving (Show, Eq, Generic)

$(instancesReadAndJSON' "G0" "LX" "CAPLightIntensity" "toInt" CAPGet)

newtype CAPMinimumWindDirection = CAPMinimumWindDirection Double deriving (Show, Eq, Generic)

$(instancesReadAndJSON' "G0" "DN" "CAPMinimumWindDirection" "toDouble" CAPGet)

newtype CAPMaximumWindDirection = CAPMaximumWindDirection Double deriving (Show, Eq, Generic)

$(instancesReadAndJSON' "G0" "DM" "CAPMaximumWindDirection" "toDouble" CAPGet)

-- The docs say this is "Dm" (lowercase m), that is incorrect, the M is uppercase.

newtype CAPAverageWindDirection = CAPAverageWindDirection Double deriving (Show, Eq, Generic)

$(instancesReadAndJSON' "G0" "DA" "CAPAverageWindDirection" "toDouble" CAPGet)

newtype CAPMinimumWindSpeed = CAPMinimumWindSpeed Double deriving (Show, Eq, Generic)

$(instancesReadAndJSON' "G0" "SN" "CAPMinimumWindSpeed" "toDouble" CAPGet)

newtype CAPMaximumWindSpeed = CAPMaximumWindSpeed Double deriving (Show, Eq, Generic)

$(instancesReadAndJSON' "G0" "SM" "CAPMaximumWindSpeed" "toDouble" CAPGet)

newtype CAPAverageWindSpeed = CAPAverageWindSpeed Double deriving (Show, Eq, Generic)

$(instancesReadAndJSON' "G0" "SA" "CAPAverageWindSpeed" "toDouble" CAPGet)

newtype CAPAccumulatedRainfall = CAPAccumulatedRainfall Double deriving (Show, Eq, Generic)

$(instancesReadAndJSON' "G0" "RA" "CAPAccumulatedRainfall" "toDouble" CAPGet)

newtype CAPRainfallDuration = CAPRainfallDuration Int deriving (Show, Eq, Generic)

$(instancesReadAndJSON' "G0" "RD" "CAPRainfallDuration" "toInt" CAPGet)

newtype CAPRainfallIntensity = CAPRainfallIntensity Double deriving (Show, Eq, Generic)

$(instancesReadAndJSON' "G0" "RI" "CAPRainfallIntensity" "toDouble" CAPGet)

newtype CAPMaximumRainfallIntensity = CAPMaximumRainfallIntensity Double deriving (Show, Eq, Generic)

$(instancesReadAndJSON' "G0" "RP" "CAPMaximumRainfallIntensity" "toDouble" CAPGet)

-- The docs say this is "Rp" (lowercase p), that is incorrect, the P is uppercase.

newtype CAPHeatingTemperature = CAPHeatingTemperature Double deriving (Show, Eq, Generic)

$(instancesReadAndJSON' "G0" "HT" "CAPHeatingTemperature" "toDouble" CAPGet)

newtype CAPFallDetection = CAPFallDetection Bool deriving (Show, Eq, Generic)

$(instancesReadAndJSON' "G0" "TILT" "CAPFallDetection" "toBool" CAPGet)

-- sensor units/update intervals

newtype CAPTemperatureUpdateInterval = CAPTemperatureUpdateInterval Int deriving (Show, Eq, Generic)

$(instancesReadAndJSON "IB" "CAPTemperatureUpdateInterval" "toInt" CAPQuery)
$(instanceWrite "IB" "CAPTemperatureUpdateInterval" $ Just "fromInt")

newtype CAPTemperatureUnit = CAPTemperatureUnit TemperatureUnit deriving (Show, Eq, Generic)

$(instancesReadAndJSON "UT" "CAPTemperatureUnit" "toTemperatureUnit" CAPQuery)
$(instanceWrite "UT" "CAPTemperatureUnit" $ Just "fromTemperatureUnit")

newtype CAPPressureUnit = CAPPressureUnit PressureUnit deriving (Show, Eq, Generic)

$(instancesReadAndJSON "UP" "CAPPressureUnit" "toPressureUnit" CAPQuery)
$(instanceWrite "UP" "CAPPressureUnit" $ Just "fromPressureUnit")

newtype CAPWindUpdateInterval = CAPWindUpdateInterval Int deriving (Show, Eq, Generic)

$(instancesReadAndJSON "IW" "CAPWindUpdateInterval" "toInt" CAPQuery)
$(instanceWrite "IW" "CAPWindUpdateInterval" $ Just "fromInt")

newtype CAPWindTimeWindow = CAPWindTimeWindow Int deriving (Show, Eq, Generic)

$(instancesReadAndJSON "AW" "CAPWindTimeWindow" "toInt" CAPQuery)
$(instanceWrite "AW" "CAPWindTimeWindow" $ Just "fromInt")

newtype CAPWindSpeedUnit = CAPWindSpeedUnit SpeedUnit deriving (Show, Eq, Generic)

$(instancesReadAndJSON "US" "CAPWindSpeedUnit" "toSpeedUnit" CAPQuery)
$(instanceWrite "US" "CAPWindSpeedUnit" $ Just "fromSpeedUnit")

newtype CAPWindOffsetCorrection = CAPWindOffsetCorrection Int deriving (Show, Eq, Generic)

$(instancesReadAndJSON "DO" "CAPWindOffsetCorrection" "toInt" CAPQuery)
$(instanceWrite "DO" "CAPWindOffsetCorrection" $ Just "fromInt")

newtype CAPRainUpdateInterval = CAPRainUpdateInterval Int deriving (Show, Eq, Generic)

$(instancesReadAndJSON "IR" "CAPRainUpdateInterval" "toInt" CAPQuery)
$(instanceWrite "IR" "CAPRainUpdateInterval" $ Just "fromInt")

newtype CAPRainUnit = CAPRainUnit LengthUnit deriving (Show, Eq, Generic)

$(instancesReadAndJSON "UR" "CAPRainUnit" "toLengthUnit" CAPQuery)
$(instanceWrite "UR" "CAPRainUnit" $ Just "fromLengthUnit")

newtype CAPRainResetMode = CAPRainResetMode RainResetMode deriving (Show, Eq, Generic)

$(instancesReadAndJSON "CR" "CAPRainResetMode" "toRainResetMode" CAPQuery)
$(instanceWrite "CR" "CAPRainResetMode" $ Just "fromRainResetMode")

newtype CAPRainOverflowValue = CAPRainOverflowValue Int deriving (Show, Eq, Generic)

$(instancesReadAndJSON "AL" "CAPRainOverflowValue" "toInt" CAPQuery)
$(instanceWrite "AL" "CAPRainOverflowValue" $ Just "fromInt")

newtype CAPRainDurationOverflowValue = CAPRainDurationOverflowValue Int deriving (Show, Eq, Generic)

$(instancesReadAndJSON "DL" "CAPRainDurationOverflowValue" "toInt" CAPQuery)
$(instanceWrite "DL" "CAPRainDurationOverflowValue" $ Just "fromInt")

newtype CAPClearRain = CAPClearRain Bool deriving (Show, Eq, Generic)

$(instancesReadAndJSON "CRA" "CAPClearRain" "toBool" CAPQuery)
$(instanceWrite "CRA" "CAPClearRain" $ Just "fromBool")

newtype CAPClearRainDuration = CAPClearRainDuration Bool deriving (Show, Eq, Generic)

$(instancesReadAndJSON "CRD" "CAPClearRainDuration" "toBool" CAPQuery)
$(instanceWrite "CRD" "CAPClearRainDuration" $ Just "fromBool")
