module WindowMessage where

-- Windows can respond to state request messages, returning an appropriate
-- value according to state. For example, a window representing a Checkbox may
-- return "On" (a StringResponse), 1 (an IntResponse), or True (a BoolResponse)
-- (Of course, this behaviour must be implemented by the programmer!)

import FlightPlan
import Waypoint
import GPS
import Coord

data StateValue = 
    StringValue String 
  | IntValue Int 
  | BoolValue Bool 
  | DoubleValue Double

  | SVCoord Coord
  | SVFlightPlan FlightPlan
  | SVWaypoint Waypoint
  | SVGPSData (Maybe (Bool,GPSData))

    deriving (Eq, Show)

globalFlightPlan = 10000 :: Int
globalSelection  = 10001 :: Int
globalGPSPosition= 10002 :: Int

mapWaypointSelection = 1 :: Int
mapWaypointSelected = 2 :: Int
mapVarPosition = 3 :: Int
