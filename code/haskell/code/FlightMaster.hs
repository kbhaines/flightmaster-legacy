-- |Module information

module Main (

    -- * Functions
  
    -- * Types

) where

import IO
import System
import System.Process
import System.Directory
import Control.Concurrent
import Control.Monad
import Control.Exception as CE
import Data.Maybe

import MessageProcessor
import Waypoint as WP
import Airspace

import GPS as GPS

import ObjectDB as DB
import Selection as Sel
import Navigation as Nav
import MapParameters as Mp
import Window as Win
import PlanEditor as PE
import Map
import Terrain
import Graphics
import GlutManager
import Control
import Utils

import Coord

import qualified UI 

-- |Command line options
data CommandLineOptions = CommandLineOptions {

    waypointsFilename :: String
  , airspaceFilename :: String
  , terrainDirectory :: String
  , gpsPort :: String
  , startAtIdent :: String
  , benchMarking :: Bool

}

--------------------------------------------------------------------------------
-- |Parse command line options
getCmdOptions :: CommandLineOptions -> [String] -> CommandLineOptions
getCmdOptions defaults [] = defaults
getCmdOptions defaults (flag:[]) = error (flag ++ " option missing an argument")
getCmdOptions defaults (flag:val:args) = 
    case flag of
     "-w"   -> getCmdOptions (defaults { waypointsFilename = val }) args
     "-a"   -> getCmdOptions (defaults { airspaceFilename = val }) args
     "-t"   -> getCmdOptions (defaults { terrainDirectory = val }) args
     "-g"   -> getCmdOptions (defaults { gpsPort = val }) args
     "-i"   -> getCmdOptions (defaults { startAtIdent = val }) args
     "-b"   -> getCmdOptions (defaults { benchMarking = True} ) args
     _ -> error ("Unrecognised option: " ++ flag)


doCommands db selection nav map = do
    cmd <- getLine
    putStrLn cmd
    hFlush stdout
    result <- commandHandler db selection nav map cmd
    putStrLn result
    hFlush stdout
    doCommands db selection nav map

commandHandler db selection nav map cmd = do
    let splitCmd = words cmd
    let subcmd = (unwords.drop 1) splitCmd

    case (splitCmd !! 0) of
        "db" -> sendToSubSystem db subcmd
        "nav" -> sendToSubSystem nav subcmd
        "sel" -> sendToSubSystem selection subcmd
        _ -> return "Invalid subsystem"

sendToSubSystem :: (Read m, Show r) => MessageProcessor m r -> String -> IO String
sendToSubSystem mp cmd = 
    case (maybeRead cmd) of
        Nothing -> return "No parse" 
        (Just c)-> do
                    res <- mp c
                    return $ show res


main = do
    commandLineArgs <- getArgs

    let userOptions = getCmdOptions (CommandLineOptions "data/waypoints.dat" "data/airspace.dat" "data/terrain/" "15" "NONE" False) commandLineArgs
        mainWindowHandleEvent win (Draw _) = (win, DrawCmds [Graphics.Circle (x,y,50) 20 (GraphicsColour3 1.0 0 0)])
            where (x,y) = dimensions win
        mainWindowHandleEvent win _ = (win, OK)

    -- load and initialise databases
    sysWpdb <- WP.loadDatabase $ waypointsFilename userOptions
    asdb <- Airspace.loadDatabase $ airspaceFilename userOptions
    trdb <- Terrain.openDB $ terrainDirectory userOptions
    textureManager <- newTextureManager $ terrainTextureLookup trdb

    --userdb <- (\l -> map WP.mkWaypoint (lines l)) `liftM` readFile "data/userwaypoints.dat"
    --gpsref <- gpsInit $ gpsPort userOptions

    gpsMP <- GPS.gpsInit $ gpsPort userOptions

    db <- newMessageProcessor (ObjectDB sysWpdb asdb) DB.interface
    selection <- newMessageProcessor Sel.initial Sel.interface
    nav <- newMessageProcessor Nav.initial Nav.interface
    mapp <- newMessageProcessor Mp.initial Mp.interface
    editor <- newMessageProcessor PE.initial PE.interface

    navExists <- doesFileExist navFile
    if navExists then do
        navData <- readFile navFile
        nav $ RestoreState navData
        return ()
      else
        return ()

    mapExists <- doesFileExist mapFile
    if mapExists then do
        mapData <- readFile mapFile
        mapp $ SetOrigin (ACOrigin $ read mapData)
        return ()
      else
        return ()

    let mapEnv = MapEnvironment db editor mapp nav selection

    window <- newMessageProcessor (UI.topWin db editor mapp nav selection ) Win.interface

    --forkOS $ doCommands db selection nav mapp

    forkOS $ mainGps gpsMP mapp
    forkOS $ saveNav navFile nav

    -- Never come back from here...
    openGLInit window textureManager (exitFM mapp nav)

--------------------------------------------------------------------------------
-- |Tidy up, save some state to files.

exitFM :: MapParametersMP -> NavigationMP -> IO ()
exitFM mapp nav = do
    (DumpResponse navStr) <- nav DumpState
    writeFile navFile navStr
    
    (OriginResponse crd) <- mapp GetOriginCoord
    writeFile mapFile (show crd)

mapFile = "data/map"
navFile = "data/nav"
--------------------------------------------------------------------------------
-- |Transfer GPS data from the GPSMP to the MapParameters MP

mainGps gpsMP mapMP = do
    (GPSResponse gps) <- gpsMP GPS.GetGPS
    case gps of
        (GPSData { GPS.position = (Just pos), GPS.velocity = (Just (speed,track)) }) -> do
            mapMP (SetACPosition $ Position pos track speed (AMSL 0))
            return ()
        _ -> return ()

    --putStrLn $ show gps
    threadDelay 1000000
    mainGps gpsMP mapMP

saveNav :: String -> NavigationMP -> IO ()
saveNav filename nav = forever $ do
    (DumpResponse navStr) <- nav DumpState
    writeFile filename navStr
    threadDelay 10000000
    --putStrLn "Dumped history"
