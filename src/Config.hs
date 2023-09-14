{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Config (readConfig, readSensors, CommunicationSettings (..), DeviceSettings (..), SensorUnits (..), CAPConfig (..)) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import System.Hardware.SenseCAP

data CommunicationSettings = CommunicationSettings
  { baud_rate :: CAPBaudRate,
    protocol :: CAPProtocol,
    modbus_address :: CAPModbusAddress,
    modbus_baudrate :: CAPRS485BaudRate
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data DeviceSettings = DeviceSettings
  { name :: CAPName,
    compass :: CAPCompassState,
    tilt_detection :: CAPTiltDetect,
    heating :: CAPHeating
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data SensorUnits = SensorUnits
  { temperature :: CAPTemperatureUnit,
    pressure :: CAPPressureUnit,
    speed :: CAPWindSpeedUnit,
    length :: CAPRainUnit
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data CAPConfig = CAPConfig
  { comms :: CommunicationSettings,
    device :: DeviceSettings,
    units :: SensorUnits
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Atmosphere = Atmosphere
  { atmTemperature :: CAPAirTemperature,
    atmHumidity    :: CAPAirHumidity,
    atmPressure    :: CAPBarometricPressure,
    atmLightIntensity :: CAPLightIntensity  
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Wind = Wind
  { windDirectionMax :: CAPMaximumWindDirection,
    windDirectionMin :: CAPMinimumWindDirection,
    windDirectionAvg :: CAPAverageWindDirection,

    windSpeedMax :: CAPMaximumWindSpeed,
    windSpeedMin :: CAPMinimumWindSpeed,
    windSpeedAvg :: CAPAverageWindSpeed
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Rain = Rain
  { accumulatedRainfall :: CAPAccumulatedRainfall,
    rainfallDuration :: CAPRainfallDuration,
    rainfallIntensity :: CAPRainfallIntensity,
    rainfallIntensityMax :: CAPMaximumRainfallIntensity
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Sensors = Sensors
  { atmosphere :: Atmosphere,
    wind :: Wind,
    rain :: Rain
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

infixl 1 <$$>

(<$$>) :: Functor f => Functor g => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

infixl 1 <$>*>

(<$>*>) :: Monad f => Applicative g => f (g (a -> b)) -> f (g a) -> f (g b)
(<$>*>) a b = a >>= bind
  where
    bind f = (f <*>) <$> b

readComms :: SenseCAP -> IO (Either String CommunicationSettings)
readComms cap =
  CommunicationSettings
    <$$> getValue cap
    <$>*> getValue cap
    <$>*> getValue cap
    <$>*> getValue cap

readDevice :: SenseCAP -> IO (Either String DeviceSettings)
readDevice cap =
  DeviceSettings
    <$$> getValue cap
    <$>*> getValue cap
    <$>*> getValue cap
    <$>*> getValue cap

readUnits :: SenseCAP -> IO (Either String SensorUnits)
readUnits cap =
  SensorUnits
    <$$> getValue cap
    <$>*> getValue cap
    <$>*> getValue cap
    <$>*> getValue cap

readConfig :: SenseCAP -> IO (Either String CAPConfig)
readConfig cap =
  CAPConfig
    <$$> readComms cap
    <$>*> readDevice cap
    <$>*> readUnits cap

readAtmosphere :: SenseCAP -> IO (Either String Atmosphere)
readAtmosphere cap = 
  Atmosphere
    <$$> getValue cap
    <$>*> getValue cap
    <$>*> getValue cap
    <$>*> getValue cap

readWind :: SenseCAP -> IO (Either String Wind)
readWind cap =
  Wind
    <$$> getValue cap
    <$>*> getValue cap
    <$>*> getValue cap
    <$>*> getValue cap
    <$>*> getValue cap
    <$>*> getValue cap

readRain :: SenseCAP -> IO (Either String Rain)
readRain cap =
  Rain
    <$$> getValue cap
    <$>*> getValue cap
    <$>*> getValue cap
    <$>*> getValue cap

readSensors :: SenseCAP -> IO (Either String Sensors)
readSensors cap =
  Sensors
    <$$> readAtmosphere cap
    <$>*> readWind cap
    <$>*> readRain cap

