-- |Map Parameters

module MapParameters (

    -- * Functions
    interface
  
    -- * Types
  , Message(..)
  , Response(..)
  , Parameters(..)
  , MapParametersMP

  , initial

  , ACPosition(..)
  , Origin(..)
  , Zoom
  , Orientation(..)
  , Filters(..)

) where

import MessageProcessor
import Coord
import Data.Maybe

type MapParametersMP = MessageProcessor Message Response

--------------------------------------------------------------------------------
-- |Parameters that control the display of the map

data Parameters = Parameters {

    acPosition   :: ACPosition 
  , origin       :: Origin
  , zoom         :: Zoom 
  , offsetVector :: Maybe Coord
  , orientation  :: Orientation
  , filters      :: Filters

}
  deriving (Eq, Show, Read)

initial = Parameters Off (ACOrigin defaultCoord) 50 Nothing North Nowt

----------------------------------------------------------------------------------
-- |Messages that the Map Processor implements

data Message = 

    -- |Set the position of the aircraft
    SetACPosition ACPosition
    
    -- |Set origin of map
  | SetOrigin Origin 

    -- |Set zoom of map (nautical miles)
  | SetZoom Zoom

    -- |Set the orientation of the map
  | SetOrientation Orientation

    -- |Set the filters for things like airspace and waypoints etc
  | SetFilters Filters

    -- |Return the origin of the map, however it is derived (ACPos, dragged etc)
  | GetOriginCoord

    -- |Compute current aircraft position
  | GetACPosition 

    -- |Set the current offset vector, to offset the centre of the map
  | SetOffsetVector (Maybe Coord)

    -- |Apply the current offset vector to the map origin

  | ApplyOffsetVector

    -- |Get all the parameters
  | GetParameters
  deriving (Eq, Show, Read)

--------------------------------------------------------------------------------
-- |Values for aircraft position

data ACPosition = 
    Off 
  | Unknown
  | Position Coord Track Speed Altitude
  | LastPosition Coord Track Speed Altitude
  deriving (Eq, Show, Read)

--------------------------------------------------------------------------------
-- |Values for origin

data Origin =
    
    -- |Origin = aircraft position if available, but the specified position
    -- if ACPosition is not available

    ACOrigin Coord

    -- |Origin = specified coordinate
  | CoordOrigin Coord
  deriving (Eq, Show, Read)

--------------------------------------------------------------------------------
-- |Map zoom 

type Zoom = NauticalMiles

--------------------------------------------------------------------------------
-- |Values for Orientation

data Orientation =
    North
  | ACTrack
  | UserDegrees Degrees
  deriving (Eq, Show, Read)

--------------------------------------------------------------------------------
-- |Values for filters

data Filters = Nowt
  deriving (Eq, Show, Read)

--------------------------------------------------------------------------------
-- |Possible responses to messages sent to the map processor

data Response = 
    BooleanResponse Bool
  | Response Parameters
  | OriginResponse Coord
  | PositionResponse (Maybe (FixAge, Coord, Track, Speed, Altitude))
  deriving (Eq, Show, Read)

type FixAge = Int -- ^Seconds since fix was made

--------------------------------------------------------------------------------
-- |Implements an interface so that MapParameters can be used as a MessageProcessor

interface :: Parameters -> Message -> (Parameters, Response)

interface params (SetACPosition pos) = (params { acPosition = pos }, BooleanResponse True)
interface params (SetOrigin newOrigin) = (params { origin = newOrigin }, BooleanResponse True)

interface params (SetZoom newZoom) 
  | newZoom <=150.0 && newZoom >= 0.1 = (params { zoom = newZoom }, BooleanResponse True)
  | otherwise = (params, BooleanResponse False)

interface params (SetOrientation o) = (params { orientation = o}, BooleanResponse True)
interface params (SetFilters f) = (params { filters = f}, BooleanResponse True)

interface params (SetOffsetVector v) = (params { offsetVector = v}, BooleanResponse True)

interface params ApplyOffsetVector = (params { origin = newOrigin, offsetVector = Nothing },  BooleanResponse True) where
    newOrigin = if offsetVector params /= Nothing then
                    CoordOrigin $ computeOrigin params
                  else
                    origin params

interface params GetOriginCoord = (params, OriginResponse $ computeOrigin params) where

interface params GetParameters = (params, Response params)

interface params@(Parameters { acPosition = Position crd track speed alt }) GetACPosition = 
    (params, PositionResponse (Just (0, crd, track, speed, alt)))

interface params@(Parameters { }) GetACPosition = (params, PositionResponse Nothing)


defaultCoord = (coordDeg 52 (-2.0))

getACCoord :: Coord -> ACPosition -> Coord
getACCoord def Off = def
getACCoord def Unknown = defaultCoord
getACCoord _ (Position crd _ _ _)  = crd
getACCoord _ (LastPosition crd _ _ _) = crd

computeOrigin :: Parameters -> Coord
computeOrigin (Parameters { acPosition = acpos, origin = o, offsetVector = ov}) = 
    case o of
        (ACOrigin def) -> (vecAdd ovector.getACCoord def) acpos
        (CoordOrigin oo) -> vecAdd ovector oo

    where 
    ovector
      | ov == Nothing  = Coord 0 0 
      | otherwise = fromJust ov
    vecAdd (Coord lat lon) (Coord vlat vlon) = Coord (lat+vlat) (lon+vlon)

