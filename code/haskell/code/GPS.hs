-------------------------------------------------------------------------------
-- GPS
-------------------------------------------------------------------------------


module GPS where

import IO
import Coord
import Utils
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.IORef
import Maybe
import Control.Concurrent
import System.Process

import Data.Time
import System.Locale
import Network

import Text.JSON

import MessageProcessor

-------------------------------------------------------------------------------
-- exported functions
--

data GPSData = GPSData {

    gpsTime  :: Maybe UTCTime           -- time reported by GPS
 ,  position :: Maybe Coord             -- position,magVarn
 ,  altitude :: Maybe Double            -- feet
 ,  velocity :: Maybe (Double, Double)  -- (speed kts, track radians)

 ,  systemTime :: UTCTime               -- time that an update was last received

 ,  history  :: Maybe GPSData           -- Last (valid) GPS fix

}
  | GPSSky
  
  deriving (Eq, Show, Read)


data GPSMessage = SetGPS GPSData | GetGPS
data GPSResponse = BooleanResponse Bool | GPSResponse GPSData deriving (Eq, Show, Read)

type GPSReference = IORef GPSData


-- |GPS Message Processor is a simple Get/Set implementation
type GPSMessageProcessor = (MessageProcessor GPSMessage GPSResponse)


initGPSData = GPSData Nothing Nothing Nothing Nothing unixEpoch Nothing

--------------------------------------------------------------------------------
-- extract track from GPSData 
--
track :: GPSData -> Double
track = snd.fromJust.velocity

--------------------------------------------------------------------------------
-- extract speed from GPSData 
--
speed :: GPSData -> Double
speed = fst.fromJust.velocity

--------------------------------------------------------------------------------
-- compute 'mode', where:
--  0 = invalid, 1 = time fix, 2 = 2d fix, 3=3d fix
--
mode :: GPSData -> Int
mode (GPSData Nothing _ _ _ _ _) = 0
mode (GPSData t Nothing _ _ _ _) = 1
mode (GPSData t p Nothing _ _ _) = 2
mode (GPSData t p a _ _ _) = 3

--------------------------------------------------------------------------------
-- GPSData is an instance of JSON, for easier integration with GPSD 
--

instance JSON GPSData where

  --readJSON :: JSValue -> Result GPSData
  --showJSON :: GPSData -> JSValue
 
  -- JSON to GPSData. Some of the fields may not be present in the JSON
  -- output, so we use do-block trickery and take advantage of Result being
  -- an instance of monad, which allows for elegant failure during parsing
 
  readJSON value@(JSObject obj) =
    case (valFromObj "class" obj) of
     (Ok "TPV") -> readTPV value
     (Ok "SKY") -> readSKY value
     _ -> Error "Unrecognised"


  readJSON _ = undefined

  -- make sure that we only output fields that we have data for in our
  -- GPSData type
  
  showJSON gps@(GPSData _ _ _ _ _ _) = makeObj $ 
                     ("class",showJSON "TPV")
                   : ("mode",showJSON $ mode gps)
                   : concat [pos.position $ gps, alt.GPS.altitude $ gps, vel.velocity $ gps, tim.gpsTime $ gps]

    where pos (Just (Coord lat lon)) = [("lat",showJSON.rad2deg $ lat),("lon",showJSON.rad2deg $ lon)]
          pos _ = []

          alt (Just a) = [("altitude", showJSON.feetToMetres $ a)]
          alt _ = []

          vel (Just (speed,track)) = [("speed",showJSON (nmToKm speed / 3600)), ("track", showJSON (rad2deg track))]
          vel _ = []

          tim (Just utc) = [("time",showJSON (fromRational $ toRational $ diffUTCTime utc unixEpoch :: Double))]
          tim _ = []

--------------------------------------------------------------------------------
-- readTPV is a helper function for readJSON
--

readTPV :: JSValue -> Result GPSData
readTPV (JSObject obj) = do
    let pos = resultToMaybe $ do
            lat <- valFromObj "lat" obj
            lon <- valFromObj "lon" obj
            return $ Coord (deg2rad lat) (deg2rad lon)

        alt = resultToMaybe $ do
            a <- valFromObj "alt" obj
            return $ metresToFeet a

        vel = resultToMaybe $ do
            s <- valFromObj "speed" obj
            t <- valFromObj "track" obj
            return (kmToNm (3600*s),deg2rad t)

        time = resultToMaybe $ do
            t <- valFromObj "time" obj
            return $ addUTCTime (fromInteger t) unixEpoch

    return $ GPSData time pos alt vel unixEpoch Nothing
    
    where
        resultToMaybe :: Result a -> Maybe a
        resultToMaybe (Ok r) = Just r
        resultToMaybe (Error _) = Nothing

--------------------------------------------------------------------------------
-- readSKY is a helper function for readJSON
--

readSKY :: JSValue -> Result GPSData
readSKY (JSObject obj) = return $ GPSSky


-- TODO: Find a better home for unixEpoch/time stuff
unixEpoch = UTCTime unixEpochDay 0
unixEpochDay = fromGregorian 1970 1 1

--------------------------------------------------------------------------------
-- |Handler for the GPS message processor. Very simple set and get primitives

gpsMessageProcessor :: GPSData -> GPSMessage -> (GPSData, GPSResponse)

gpsMessageProcessor gps GetGPS = (gps, GPSResponse gps)
gpsMessageProcessor gps (SetGPS new) = (new, GPSResponse gps)

--------------------------------------------------------------------------------
-- |GPS client, storing updates in a MessageProcessor.  The client comprises a
--couple of threads, one is for reading from the gpsd-compatible JSON-stream
--(over the network) and the other is used to watch that the stream doesn't dry
--up and need to be restarted.

gpsClient :: String -> Int -> IO GPSMessageProcessor
gpsClient ipAddr port = do
    gpsMP <- newMessageProcessor initGPSData gpsMessageProcessor

    clientHandle <- connectTo ipAddr (PortNumber $ fromIntegral port)

    -- thread 1 - handling incoming data from network
    forkIO $ do
        hSetBuffering clientHandle LineBuffering
        hPutStrLn clientHandle "?WATCH={\"enable\":true, \"json\":true}"
        gpsFeed <- hGetContents clientHandle
        mapM (\x -> updateGPSData gpsMP x) (lines gpsFeed)
        return ()

    -- thread 2 - watch that the data stream doesn't dry up
    forkIO $ gpsMonitor gpsMP clientHandle
        
    return gpsMP

    where
        updateGPSData gpsd jsonInput = do
            
            -- TODO: Be aware of the potential for funny conditions caused by
            -- simultaneous updates to the GPS data. Probably not an issue
            -- unless we're going to allow that.
            
            (GPSResponse currentGPS) <- gpsd GetGPS
            currentTime <- getCurrentTime

            -- History management. History is only updated when the current
            -- position is valid, otherwise it is left alone
            
            let newHistory = if position currentGPS /= Nothing then
                                Just $ currentGPS { history = Nothing }
                              else
                                history currentGPS

                newGPS = case (decode jsonInput :: Result GPSData) of
                            (Ok GPSSky) -> currentGPS
                            (Ok g) -> g { systemTime = currentTime, history = newHistory }
                            --(Error _) -> initGPSData { history = Just $ currentGPS { history = Nothing }}
                            (Error _) -> currentGPS

            if newGPS /= currentGPS then do
                debug $ "Updated GPS" ++ (show $ (gpsTime newGPS, position newGPS))
                hFlush stdout
              else
                return ()
            resp <- gpsd (SetGPS newGPS)
            return ()

-- | Process that monitors the systemTime of GPSData to determine if the data
-- stream has dried up. This can happen when the user unplugs the GPS for example.
-- 
-- When the stream dries up, the current fix is overridden with non-fix data 
-- (initGPSData) and the process enters a 5-second retry interval in an 
-- attempt to restart the GPS.
gpsMonitor :: GPSMessageProcessor -> Handle -> IO ()
gpsMonitor gpsd clientHandle = do
    now <- getCurrentTime
    (GPSResponse currentGPS) <- gpsd GetGPS
    if dataStreamAge currentGPS now > 3 then do
       res <- gpsd (SetGPS $ initGPSData { history = history currentGPS, systemTime = now })
       hPutStrLn clientHandle "?WATCH={\"enable\":true, \"json\":true}"
       debug "GPS-Restart"
       threadDelay 5000000
     else
       threadDelay 3000000

    gpsMonitor gpsd clientHandle

--------------------------------------------------------------------------------
-- |Initialise a new GPS by trying to connect to a GPS Daemon. If one isn't
-- there (e.g. gpsd), then start our own process and retry the connection

gpsInit :: String -> IO GPS.GPSMessageProcessor
gpsInit port = do
    gpsRef <- gpsClient "127.0.0.1" 2947
    return gpsRef
    `IO.catch` (\e -> do
        --debug "No GPS server, starting my own..."
        (_,_,_,ph) <- createProcess (proc "./GPSReader" [port])
        threadDelay 200000
        gpsRef <- GPS.gpsClient "127.0.0.1" 2947
        return gpsRef
     )
        

--------------------------------------------------------------------------------
-- calculate age of GPS data based on system time and last received data
--

dataStreamAge :: GPSData -> UTCTime -> Integer
dataStreamAge gpsData sysTime = truncate $ diffUTCTime sysTime (systemTime gpsData)

-------------------------------------------------------------------------------
-- private functions & data
--
--

