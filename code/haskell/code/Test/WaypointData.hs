module Test.WaypointData

where

import Waypoint
import Coord
import Data.List
import Data.Maybe
import Utils

-- list of unWaypoints
waypoints = ["EGBP N51400500W002032500 KEMBLE","DTY N52104900W001065000 DAVENTRY","EGBE N52221100W001284700 COVENTRY","EGLL N51283900W000274100 HEATHROW","EGBJ N51533900W002100200 GLOUCESTERSHIRE", "EGVN N51445985W001350110 BRIZE","CPT N51293000W001131100 COMPTON","EGGD N51225761W002430871 BRISTOL","EGBS N52143000W002525200 SHOBDON","BCN N41182563E002062810 BARCELONA","BCN N51433200W003154700 BRECON"]

egbpUnpack = waypoints !! 0
dtyUnpack = waypoints !! 1
egbeUnpack = waypoints !! 2
egllUnpack = waypoints !! 3
egbjUnpack = waypoints !! 4

-- test helper functions
mkc = (mkCoord.mkSString)

mkwn = mkWaypoint.searchByName
mkwi = mkWaypoint.head.searchUnWaypoints

wplist = map mkWaypoint waypoints
wpdatabase = mkDatabase wplist

--unWp :: String -> String
--unWp id = find (\i -> (words i)!!0 == id) waypoints

-- return subset of unWaypoints by identifier
searchUnWaypoints :: String -> [String]
searchUnWaypoints id = filter (\i -> (words i)!!0 == id) waypoints

searchByName :: String -> String
searchByName name 
    | res /= Nothing = fromJust res
    | otherwise = ""
    where res = find (\i -> (words i)!!2 == name) waypoints

