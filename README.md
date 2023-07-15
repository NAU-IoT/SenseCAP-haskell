# SenseCAP API/CLI Tool

This is a command-line and Haskell API tool for interacting with and debugging the SenseCAP
weather sensors from Seeed Studio. Most of the time the CLI should provide sufficient access
to the sensor.


## CLI Usage

```
weatherstation [COMMAND] ... [OPTIONS]

Common flags:
  -p --port=PORT       Serial port to connect to. defaults to /dev/ttyUSB0.
  -d --device=DEVICE   Device ID of the SenseCAP. This is almost always 0
                       (default).
  -b --baud=COMMSPEED  Baudrate of serial connection (9600 default).
  -? --help            Display help message
  -V --version         Print version information

weatherstation repl [OPTIONS]
  Launch a REPL to issue commands to and interact with the sensor. Useful for
  debugging the sensor.

weatherstation query [OPTIONS]
  Query values from the sensor using a config file.

  -c --config=ITEM     Config file to use. Defaults to config.yml.
```

## API Usage

The API is very rudimentary, best explained by example:

```haskell
import System.Hardware.SenseCAP (withSenseCAP, getSenseCAP, querySenseCAP, setSenseCAP)
import System.Hardware.Serialport (CommSpeed (..))

unwrap :: String -> Maybe String -> String
unwrap message = maybe "An error occurred!" (<> message) 

main :: IO ()
main = withSenseCAP "/dev/ttyUSB0" 0 CS9600 $ \cap -> do
  res <- getSenseCAP cap "G0" -- get all sensor values
  putStrLn $ unwrap "All sensor values: " res
  res2 <- querySenseCAP cap "UT" -- query temperature unit (Since temperature unit is read/write, we must use QUERY not GET).
  putStrLn $ unwrap "Temperature unit: " res2
  res3 <- setSenseCAP cap "UT" "F" -- set temperature unit to Farenheit (command returns new unit)
  putStrLn $ unwrap "Temperature unit: " res3

```

Example output:
```
All sensor values: AT=23.37;AH=27.40;AP=79520;LX=325;DN=0.0;DM=333.4;DA=0.0;SN=0.0;SM=0.3;SA=0.0;RA=0.0;RD=0;RI=0.0;RP=0.0;PM2.5=0;PM10=0;HT=22.86;TILT=0;BMP_T=0.00;BMP_P=0.00;BMP_RH=0.00;SA_CO2=359;SA_CO2_RAW=363;SA_T=24.32;CO2=359
Temperature unit: UT=C
Temperature unit: UT=F
```

## Common issues

### No response on serial port

Ensure the sensor is configured correctly. 

* Set the sensor to RS-485 ASCII mode (**NOT** RS-485 Modbus!) via the
  [configuration app](https://github.com/Seeed-Solution/SenseCAP-One-Configuration-Tool).
* Ensure the baud rate matches the sensor's (check this in the config app).
* Ensure the sensor is powered on. (Can you access it over USB with the config app?)
* Can you access the sensor over USB with this utility? (The USB interface is also a serial connection,
  so you should be able to interact with it using this utility!)
* Ensure the correct connections are made:
  * White wire = RS-485 T/R+
  * Blue wire = RS-485 T/R-

### Gibberish on serial port
* Ensure the baud rate is correctly set on the sensor and the host. (They must match)
* Ensure your program/the command-line utility are getting the file handle to themselves.
  A simple way to verify this is by attempting to write to the file while the program
  is running. This **should** fail. e.g.
  ```
  [dfsek@dfsek-laptop ~]$ echo "test" > /dev/ttyUSB0
  -bash: /dev/ttyUSB0: Device or resource busy
  ```
  If this succeeds, the program is not acquiring the file handle, this means it is unable
  to properly configure parameters of the serial connection.

### Permission denied for `/dev/ttyUSB0`
By default, on most distributions, users cannot access serial ports. You must be a member of a certain
group to access serial ports, this group varies per distro, but you can find it
using `stat /dev/ttyUSB0`. Example output:
```
  File: /dev/ttyUSB0
  Size: 0               Blocks: 0          IO Block: 4096   character special file
Device: 0,5     Inode: 957         Links: 1     Device type: 188,0
Access: (0660/crw-rw----)  Uid: (    0/    root)   Gid: (  986/    uucp)
```

The important part here is `Gid: (  986/    uucp)`. To add myself to this group
I would use `usermod -a -G uucp dfsek`. Obviously replace `dfsek` with your username.

After doing this you will need to sign out/sign back in for it to take effect.

Common groups for dictating serial port access are:
 * `uucp`
 * `dialout`
 * `tty`
