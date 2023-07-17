{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Config (readConfig, CommunicationSettings (..), DeviceSettings (..), SensorUnits (..), CAPConfig (..)) where

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
