module Main where

import IO
import System.IO
import qualified SerialController
import qualified Data.ByteString.Lazy.Char8 as BSC
import Control.Concurrent
import Control.Concurrent.Chan
import Network
import qualified Control.Exception as CE
import System
import System.Locale
import Data.Time
import Data.IORef
import Text.JSON as JSON
import Maybe

import GPS
import Utils
import Coord

main = withSocketsDo $ do 

    args <- getArgs

    -- listen for client, and wait until it connects. Then get the
    -- client command (and discard it, we don't need it)
    sck <- listenOn (PortNumber 2947)

    (clientHandle,_,_) <- accept sck
    hSetBuffering clientHandle LineBuffering
    _ <- hGetLine clientHandle

    gpsData <- newIORef initGPSData
    
    -- start a thread for the reader process that handles communication
    -- with the actual GPS. It will store its results in the gpsData
    -- IOReference...
    readerThread <- forkIO $ gpsReaderLoop gpsData (args !! 0) 

    -- ...which is read by this process, that converts it to JSON and squirts
    -- it back to the client. When the client disconnects, it's game-over
    -- for us!
    gpsJSONOutput gpsData clientHandle

    `catch` (\e -> do
       putStrLn "Port aleady in use"
    )

--------------------------------------------------------------------------------
-- gpsReaderLoop updates a GPSData structure wrapped inside an IORef.
--
-- The structure is updated in accordance with the NMEA sentences received from
-- the GPS at the specified port.
-- 

gpsReaderLoop :: IORef GPSData -> String -> IO ()
gpsReaderLoop gpsData portId = do
    debug "Opening port"
    (gpsHandle,killSwitch) <- SerialController.openPort portId

    monitorThread <- forkIO $ monitor killSwitch

    hSetBuffering gpsHandle LineBuffering
    g <- BSC.hGetContents gpsHandle
    mapM (\x -> updateGPSData gpsData x) (BSC.lines g)

    -- in normal operations we should not get past this point, however
    -- if comms errors occur and the external process (opened by serialController)
    -- dies then we should attempt a restart.
    --
    -- NB this can also occur if the monitoring thread detects a timeout and
    -- calls killSwitch to close the GPS comms channel

    killThread monitorThread
    writeIORef gpsData initGPSData
    debug "GPS lost, restarting in 5 seconds"
    threadDelay $ 5000000
    gpsReaderLoop gpsData portId

    where
        -- the update process accepts NMEA sentences and converts them into
        -- the GPSData structure
        updateGPSData :: IORef GPSData -> BSC.ByteString -> IO ()
        updateGPSData gpsd nmea = 
            --(debug $ show nmea) >>
            getCurrentTime >>= \sysTime ->
            readIORef gpsd >>= \lastFix ->

            -- TODO: Possible bug because GPSData now has two constructors...
            let currFix = ((parse lastFix nmea) { systemTime = sysTime }) in
                writeIORef gpsd currFix

        -- The monitoring thread deals with the cases where a GPS receiver stops 
        -- sending updates - after 10 seconds the monitoring thread will call 
        -- the killSwitch
        monitor :: IO () -> IO ()
        monitor killSwitch = do
                    threadDelay 10000000
                    gpsData' <- readIORef gpsData
                    now <- getCurrentTime
                    if dataStreamAge gpsData' now > 10 then
                        killSwitch
                      else
                        monitor killSwitch

--------------------------------------------------------------------------------
-- send JSON output to the client every second, until an exception occurs
--

gpsJSONOutput :: GPSReference -> Handle -> IO ()
gpsJSONOutput gpsRef clientHandle = do
    gps <- readIORef gpsRef
    now <- getCurrentTime

    let clientMsg = if (dataStreamAge gps now < 4) then
                        gps { systemTime = now }
                      else
                        initGPSData

    hPutStrLn clientHandle $ JSON.encode $ clientMsg
    hFlush clientHandle

    --debug $ JSON.encode clientMsg
    --debug $ show $ (JSON.decode.JSON.encode $ clientMsg :: Result GPSData)

    threadDelay 1000000
    gpsJSONOutput gpsRef clientHandle
    `catch` 
    (\e -> do 
        debug "Client disconnect"
        return ()
    )


-------------------------------------------------------------------------------
-- NMEA parsing

parse :: GPSData -> BSC.ByteString -> GPSData
parse olddata s = 
    -- TODO: validate the checksum
    case (tokens !! 0) of
       "$GPRMC" -> parseRMC olddata tokens
       "$GPGGA" -> parseGGA olddata tokens
       "$GPGSA" -> parseGSA olddata tokens
       _ -> olddata
    where tokens = splitString ',' (BSC.unpack s)

-------------------------------------------------------------------------------

allowNullDefault :: String -> Double -> Maybe Double
allowNullDefault str def = 
    if str == "" then 
        Just def
     else 
        (readM (str) :: (Maybe Double) )

-------------------------------------------------------------------------------

parseRMC :: GPSData -> [String] -> GPSData
parseRMC olddata tokens 
 |  (tokens !! 2) == "A" && length tokens > 9 && parseFix /= Nothing=
    fromJust parseFix
 |  newTime /= Nothing = GPSData newTime Nothing Nothing Nothing unixEpoch Nothing
 |  otherwise = olddata
 where newTime = parseTime defaultTimeLocale "%d%m%Y%H%M%S%Q" 
            ((take 4 $ tokens !! 9) ++ "20" ++ (drop 4 $ tokens !! 9) ++ tokens !! 1)
       parseFix = do
        c <- nmeaCoord (tokens !! 3, tokens !! 4) (tokens !! 5, tokens !! 6)
        sp <- allowNullDefault (tokens !! 7) 0.0 
        th <- allowNullDefault (tokens !! 8) 0.0
        mv <- allowNullDefault (tokens !! 10) 0.0
        return $ olddata { gpsTime=newTime, position=Just c, velocity = Just (sp, th) }


-------------------------------------------------------------------------------

parseGGA :: GPSData -> [String] -> GPSData
parseGGA oldData tokens 
 |  (length tokens < 12) = oldData
 |  (tokens !! 10) /= "M" = oldData
 |  otherwise = 
    case (allowNullDefault (tokens !! 9) 0.0) of
        Nothing -> oldData
        (Just a) -> oldData { altitude = Just (metresToFeet a) } 

-------------------------------------------------------------------------------

parseGSA :: GPSData -> [String] -> GPSData
parseGSA oldData tokens
 |  (length tokens < 18) = oldData
 |  (tokens !! 2 == "1") = oldData { position = Nothing, velocity = Nothing, altitude = Nothing }
 |  (tokens !! 2 == "2") = oldData { altitude = Nothing }
 |  otherwise = oldData

-------------------------------------------------------------------------------

-- TODO: Re-implement using Parsec
nmeaCoord :: (String,String) -> (String,String) -> Maybe Coord
nmeaCoord (lat,lathem) (lon,lonhem) 
 |(length lat == 9 && length lon == 10) = 
    (readM lat :: (Maybe Double)) >>= \latdm ->
    (readM lon :: (Maybe Double)) >>= \londm ->
    (latHemCheck lathem (locc latdm)) >>= \latd ->
    (lonHemCheck lonhem (locc londm)) >>= \lond ->
    if ( latd >= (-90.0) && latd <= 90.0 && lond >= (-180.0) && lond <= 180) then
        return $ Coord (deg2rad latd) (deg2rad lond)
      else 
        Nothing

  |otherwise = Nothing

    where
        locc :: Double -> Double
        locc nmea = d + m/60
          where
            (d,m) = (fromIntegral $ truncate (nmea / 100), (nmea-d*100))

-------------------------------------------------------------------------------

latHemCheck :: String -> Double -> Maybe Double
latHemCheck hem degs =
    case hem of 
        "N" -> Just degs
        "S" -> Just (-degs)
        _ -> Nothing

-------------------------------------------------------------------------------

lonHemCheck :: String -> Double -> Maybe Double
lonHemCheck hem degs =
    case hem of 
        "E" -> Just degs
        "W" -> Just (-degs)
        _ -> Nothing

-------------------------------------------------------------------------------

