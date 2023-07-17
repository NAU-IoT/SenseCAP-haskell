module Config (readComms) where

import System.Hardware.SenseCAP

data CommunicationSettings = CommunicationSettings
  { baud_rate :: CAPBaudRate,
    protocol :: CAPProtocol,
    modbus_address :: CAPModbusAddress,
    modbus_baudrate :: CAPRS485BaudRate
  } deriving (Show, Eq)

data DeviceSettings = DeviceSettings
  { name :: CAPName,
    compass :: CAPCompassState,
    tilt_detection :: CAPTiltDetect,
    heating :: CAPHeating
  }

data SensorUnits = SensorUnits
  { temperature :: TemperatureUnit,
    pressure :: PressureUnit,
    speed :: SpeedUnit,
    length :: LengthUnit
  }

infixl 1 <$$>

(<$$>) :: Functor f => Functor g => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

infixl 1 <$>*>

(<$>*>) :: Monad f => Applicative g => f (g (a -> b)) -> f (g a) -> f (g b)
(<$>*>) a b = a >>= bind
  where
    bind f = fmap (f <*>) b

readComms :: SenseCAP -> IO (Either String CommunicationSettings)
readComms cap =
  CommunicationSettings
    <$$> getValue cap
    <$>*> getValue cap
    <$>*> getValue cap
    <$>*> getValue cap
